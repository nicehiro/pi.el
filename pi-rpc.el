;;; pi-rpc.el --- RPC transport for pi subprocesses -*- lexical-binding: t; -*-

;; Author: Fangyuan and contributors
;; Keywords: tools, processes
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Low-level JSONL RPC transport for `pi --mode rpc`.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup pi-rpc nil
  "Low-level RPC transport for pi."
  :group 'applications)

(defcustom pi-rpc-executable "pi"
  "Executable used to launch pi in RPC mode."
  :type 'string)

(defcustom pi-rpc-extra-args nil
  "Extra command-line arguments passed to pi in RPC mode."
  :type '(repeat string))

(defcustom pi-rpc-debug-buffer t
  "Whether to create an internal process buffer for each RPC subprocess."
  :type 'boolean)

(cl-defstruct (pi-rpc
               (:constructor pi-rpc--create)
               (:copier nil))
  process
  buffer
  (next-id 0)
  pending-by-id
  event-handler
  exit-handler
  stdout-fragment
  (alive-p nil))

(defun pi-rpc--generate-id (rpc)
  "Generate the next request id for RPC."
  (setf (pi-rpc-next-id rpc) (1+ (pi-rpc-next-id rpc)))
  (format "req_%d" (pi-rpc-next-id rpc)))

(defun pi-rpc--make-process-buffer (name)
  "Create the internal process buffer for NAME if enabled."
  (when pi-rpc-debug-buffer
    (generate-new-buffer (format " *pi-rpc:%s*" name))))

(defun pi-rpc--normalize-command (command)
  "Convert COMMAND into a JSON object alist.
COMMAND may be an alist or plist."
  (cond
   ((null command) nil)
   ((and (listp command) (keywordp (car command)))
    (let (result)
      (while command
        (let* ((raw-key (pop command))
               (value (pop command))
               (key (intern (substring (symbol-name raw-key) 1))))
          (push (cons key value) result)))
      (nreverse result)))
   ((listp command)
    (mapcar (lambda (entry)
              (pcase-let ((`(,key . ,value) entry))
                (cons (cond
                       ((symbolp key) key)
                       ((stringp key) (intern key))
                       (t (error "Unsupported RPC key: %S" key)))
                      value)))
            command))
   (t (error "Unsupported RPC command shape: %S" command))))

(defun pi-rpc--encode-command (command)
  "Encode COMMAND as a JSONL string."
  (let ((json-false :json-false)
        (json-null nil))
    (concat (json-encode command) "\n")))

(defun pi-rpc--call-handler (fn &rest args)
  "Call FN with ARGS, ignoring nil handlers."
  (when fn
    (apply fn args)))

(defun pi-rpc--strip-leading-terminal-noise (line)
  "Strip leading terminal control sequences from LINE."
  (let ((result line)
        (changed t))
    (while changed
      (setq changed nil)
      ;; OSC ... BEL or ESC \
      (when (string-match (rx string-start "\e]" (*? anything) (or "\a" "\e\\")) result)
        (setq result (substring result (match-end 0)))
        (setq changed t))
      ;; CSI sequence
      (when (string-match (rx string-start "\e[" (* (any "0-9:;<=>?")) (* (any " -/")) (any "@-~")) result)
        (setq result (substring result (match-end 0)))
        (setq changed t))
      ;; Other stray control chars before payload
      (when (string-match (rx string-start (+ (any "\r\n\t\f\v"))) result)
        (setq result (substring result (match-end 0)))
        (setq changed t)))
    result))

(defun pi-rpc--flush-line (rpc line)
  "Handle one decoded JSONL LINE for RPC."
  (let ((clean-line (pi-rpc--strip-leading-terminal-noise line)))
    (if (not (string-prefix-p "{" clean-line))
        (pi-rpc--call-handler
         (pi-rpc-event-handler rpc)
         rpc
         (list :type "rpc_output"
               :line line
               :clean-line clean-line))
      (condition-case err
          (let* ((json-object-type 'plist)
                 (json-array-type 'list)
                 (json-false :json-false)
                 (json-null nil)
                 (payload (json-parse-string clean-line :object-type 'plist :array-type 'list
                                             :false-object :json-false :null-object nil))
                 (type (plist-get payload :type)))
            (if (equal type "response")
                (let* ((id (plist-get payload :id))
                       (pending (and id (gethash id (pi-rpc-pending-by-id rpc)))))
                  (when id
                    (remhash id (pi-rpc-pending-by-id rpc)))
                  (if pending
                      (funcall pending payload)
                    (pi-rpc--call-handler
                     (pi-rpc-event-handler rpc)
                     rpc
                     (list :type "rpc_unmatched_response"
                           :payload payload))))
              (pi-rpc--call-handler (pi-rpc-event-handler rpc) rpc payload)))
        (error
         (pi-rpc--call-handler
          (pi-rpc-event-handler rpc)
          rpc
          (list :type "rpc_parse_error"
                :error (error-message-string err)
                :line line
                :clean-line clean-line)))))))

(defun pi-rpc--process-filter (process chunk)
  "Process filter for pi RPC PROCESS receiving CHUNK."
  (let* ((rpc (process-get process 'pi-rpc-object))
         (fragment (concat (or (pi-rpc-stdout-fragment rpc) "") chunk))
         (lines nil)
         (start 0))
    (while (string-match "\n" fragment start)
      (let ((end (match-beginning 0)))
        (push (substring fragment start end) lines)
        (setq start (match-end 0))))
    (setf (pi-rpc-stdout-fragment rpc) (substring fragment start))
    (dolist (line (nreverse lines))
      (when (string-suffix-p "\r" line)
        (setq line (substring line 0 -1)))
      (unless (string-empty-p line)
        (pi-rpc--flush-line rpc line)))))

(defun pi-rpc--fail-pending (rpc reason)
  "Fail all pending requests on RPC with REASON."
  (maphash
   (lambda (_id callback)
     (funcall callback
              (list :type "response"
                    :command "process_exit"
                    :success :json-false
                    :error reason)))
   (pi-rpc-pending-by-id rpc))
  (clrhash (pi-rpc-pending-by-id rpc)))

(defun pi-rpc--process-sentinel (process event)
  "Sentinel for RPC PROCESS receiving EVENT."
  (let ((rpc (process-get process 'pi-rpc-object)))
    (unless (process-live-p process)
      (setf (pi-rpc-alive-p rpc) nil)
      (let ((fragment (pi-rpc-stdout-fragment rpc)))
        (when (and fragment (not (string-empty-p fragment)))
          (pi-rpc--flush-line rpc fragment)
          (setf (pi-rpc-stdout-fragment rpc) nil)))
      (pi-rpc--fail-pending rpc (string-trim event))
      (pi-rpc--call-handler (pi-rpc-exit-handler rpc) rpc event))))

(defun pi-rpc-start (cwd &optional name on-event on-exit)
  "Start a pi RPC subprocess in CWD.
NAME is used for the internal process and buffer names.
ON-EVENT receives (RPC EVENT).
ON-EXIT receives (RPC EVENT)."
  (let* ((default-directory (file-name-as-directory (expand-file-name cwd)))
         (buffer (pi-rpc--make-process-buffer (or name "default")))
         (rpc (pi-rpc--create
               :buffer buffer
               :pending-by-id (make-hash-table :test #'equal)
               :event-handler on-event
               :exit-handler on-exit
               :stdout-fragment ""
               :alive-p t))
         (process (make-process
                   :name (format "pi-rpc-%s" (or name "default"))
                   :buffer buffer
                   :command (append (list pi-rpc-executable "--mode" "rpc")
                                    pi-rpc-extra-args)
                   :coding 'utf-8-unix
                   :connection-type 'pipe
                   :noquery t
                   :filter #'pi-rpc--process-filter
                   :sentinel #'pi-rpc--process-sentinel)))
    (set-process-query-on-exit-flag process nil)
    (process-put process 'pi-rpc-object rpc)
    (setf (pi-rpc-process rpc) process)
    rpc))

(defun pi-rpc-stop (rpc)
  "Stop RPC subprocess RPC."
  (when (and rpc (process-live-p (pi-rpc-process rpc)))
    (delete-process (pi-rpc-process rpc)))
  (setf (pi-rpc-alive-p rpc) nil)
  t)

(defun pi-rpc-live-p (rpc)
  "Return non-nil if RPC subprocess is alive."
  (and rpc
       (pi-rpc-alive-p rpc)
       (process-live-p (pi-rpc-process rpc))))

(defun pi-rpc-send (rpc command callback)
  "Send COMMAND over RPC and invoke CALLBACK with the decoded response.
COMMAND may be a plist or alist.  Returns the generated request id."
  (unless (pi-rpc-live-p rpc)
    (error "RPC process is not running"))
  (let* ((id (pi-rpc--generate-id rpc))
         (normalized (pi-rpc--normalize-command command))
         (command-with-id (append normalized (list (cons 'id id)))))
    (puthash id (or callback #'ignore) (pi-rpc-pending-by-id rpc))
    (process-send-string (pi-rpc-process rpc)
                         (pi-rpc--encode-command command-with-id))
    id))

(defun pi-rpc-notify (rpc command)
  "Send COMMAND over RPC without tracking a response.
COMMAND may be a plist or alist."
  (unless (pi-rpc-live-p rpc)
    (error "RPC process is not running"))
  (process-send-string (pi-rpc-process rpc)
                       (pi-rpc--encode-command
                        (pi-rpc--normalize-command command)))
  t)

(defun pi-rpc-set-event-handler (rpc fn)
  "Set RPC event handler FN for RPC."
  (setf (pi-rpc-event-handler rpc) fn))

(provide 'pi-rpc)

;;; pi-rpc.el ends here
