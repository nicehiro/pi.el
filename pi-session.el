;;; pi-session.el --- Session management for pi RPC subprocesses -*- lexical-binding: t; -*-

;; Author: Fangyuan and contributors
;; Keywords: tools, processes
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Hidden session management and routing for `pi --mode rpc`.
;;
;; Session model:
;; - scope = nearest git root, else current directory
;; - one session per scope
;; - prompts route to current buffer scope

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pi-rpc)

(defgroup pi-session nil
  "Session management for pi."
  :group 'applications)

(defcustom pi-session-idle-timeout-seconds (* 10 60)
  "Number of idle seconds before a hidden pi subprocess is shut down."
  :type 'integer)

(defcustom pi-session-keepalive-predicate nil
  "Optional predicate to keep an idle SESSION alive.

When non-nil, this function is called with one argument SESSION before an
idle shutdown. If it returns non-nil, the session is kept alive and the idle
timer is rescheduled."
  :type '(choice (const :tag "None" nil) function))

(defcustom pi-session-agent-directory (expand-file-name "~/.pi/agent")
  "Base directory used by pi to persist session files."
  :type 'directory)

(defcustom pi-session-active-index-file
  (locate-user-emacs-file "pi-active-sessions.eld")
  "File used to persist the active session file for each scope."
  :type 'file)

(defcustom pi-session-saved-session-scan-bytes (* 32 1024)
  "Maximum bytes to inspect from the start and end of a session file.

Used when building the resume list so Emacs does not read entire session
transcripts just to show picker labels."
  :type 'integer)

(cl-defstruct (pi-session
               (:constructor pi-session--create)
               (:copier nil))
  id
  name
  scope
  scope-key
  root
  session-file
  session-id
  rpc
  cached-state
  ready-callbacks
  last-used-at
  idle-timer
  status)

(defvar pi-session-event-hook nil
  "Hook run with `(SESSION EVENT)` for session-level RPC events.")

(defvar pi-session--by-scope (make-hash-table :test #'equal))
(defvar pi-session--known-sessions nil)
(defvar pi-session--next-id 0)
(defvar pi-session--active-index :uninitialized)

(defun pi-session--next-id ()
  (setq pi-session--next-id (1+ pi-session--next-id))
  (format "session-%d" pi-session--next-id))

(defun pi-session--normalize-dir (dir)
  (let ((path (directory-file-name
               (file-truename
                (file-name-as-directory
                 (expand-file-name (or dir default-directory)))))))
    (if (string-empty-p path) "/" path)))

(defun pi-session--load-active-index ()
  (unless (hash-table-p pi-session--active-index)
    (let ((table (make-hash-table :test #'equal)))
      (when (file-readable-p pi-session-active-index-file)
        (condition-case nil
            (dolist (entry (with-temp-buffer
                             (insert-file-contents pi-session-active-index-file)
                             (read (current-buffer))))
              (when (and (consp entry)
                         (stringp (car entry))
                         (stringp (cdr entry)))
                (puthash (car entry) (cdr entry) table)))
          (error nil)))
      (setq pi-session--active-index table)))
  pi-session--active-index)

(defun pi-session--save-active-index ()
  (let (entries)
    (maphash (lambda (scope-key session-file)
               (push (cons scope-key session-file) entries))
             (pi-session--load-active-index))
    (make-directory (file-name-directory pi-session-active-index-file) t)
    (with-temp-file pi-session-active-index-file
      (let ((print-length nil)
            (print-level nil))
        (prin1 (sort entries (lambda (a b)
                               (string-lessp (car a) (car b))))
               (current-buffer))
        (insert "\n")))))

(defun pi-session--set-active-file (scope-key session-file)
  (let ((table (pi-session--load-active-index)))
    (if (and (stringp session-file)
             (file-exists-p session-file))
        (puthash scope-key session-file table)
      (remhash scope-key table))
    (pi-session--save-active-index)))

(defun pi-session--get-active-file (scope-key)
  (let* ((table (pi-session--load-active-index))
         (session-file (gethash scope-key table)))
    (cond
     ((not (stringp session-file)) nil)
     ((file-exists-p session-file) session-file)
     (t
      (remhash scope-key table)
      (pi-session--save-active-index)
      nil))))

(defun pi-session--session-dir-for-root (root)
  (let* ((normalized-root (pi-session--normalize-dir root))
         (safe-root (format "--%s--"
                            (replace-regexp-in-string
                             "[/\\:]" "-"
                             (replace-regexp-in-string "\\`[/\\]" ""
                                                       normalized-root))))
         (session-dir (expand-file-name (concat "sessions/" safe-root)
                                        pi-session-agent-directory)))
    (when (file-directory-p session-dir)
      session-dir)))

(defun pi-session--json-line-object (line)
  (json-parse-string line
                     :object-type 'plist
                     :array-type 'list
                     :false-object :json-false
                     :null-object nil))

(defun pi-session--message-text (message)
  (let ((content (plist-get message :content)))
    (string-trim
     (cond
      ((stringp content) content)
      ((listp content)
       (string-join
        (delq nil
              (mapcar (lambda (block)
                        (when (and (listp block)
                                   (equal (plist-get block :type) "text"))
                          (let ((text (plist-get block :text)))
                            (and (stringp text) text))))
                      content))
        "\n"))
      (t "")))))

(defun pi-session--read-jsonl-slice (path start end file-size)
  (let (entries)
    (with-temp-buffer
      (insert-file-contents path nil start end)
      (when (> start 0)
        (goto-char (point-min))
        (if (search-forward "\n" nil t)
            (delete-region (point-min) (point))
          (erase-buffer)))
      (when (< end file-size)
        (goto-char (point-max))
        (unless (or (bobp)
                    (eq (char-before) ?\n))
          (delete-region (line-beginning-position) (point-max))))
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (string-trim
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
               (entry (and (not (string-empty-p line))
                           (condition-case nil
                               (pi-session--json-line-object line)
                             (error nil)))))
          (when entry
            (push entry entries)))
        (forward-line 1)))
    (nreverse entries)))

(defun pi-session--saved-session-info (path)
  (condition-case nil
      (let* ((attrs (file-attributes path 'string))
             (modified (file-attribute-modification-time attrs))
             (file-size (file-attribute-size attrs))
             (scan-bytes (max 1024 pi-session-saved-session-scan-bytes))
             (head-end (min file-size scan-bytes))
             (tail-start (max 0 (- file-size scan-bytes)))
             (head-entries (pi-session--read-jsonl-slice path 0 head-end file-size))
             (tail-entries (if (> tail-start 0)
                               (pi-session--read-jsonl-slice path tail-start file-size file-size)
                             head-entries))
             session-id
             cwd
             name
             preview)
        (dolist (entry head-entries)
          (pcase (plist-get entry :type)
            ("session"
             (setq session-id (or session-id (plist-get entry :id))
                   cwd (or cwd (plist-get entry :cwd))))
            ("session_info"
             (when-let* ((session-name (plist-get entry :name))
                         ((stringp session-name))
                         (session-name (string-trim session-name))
                         ((not (string-empty-p session-name))))
               (setq name session-name)))
            ("message"
             (let* ((message (plist-get entry :message))
                    (role (plist-get message :role)))
               (when (and (member role '("user" "assistant"))
                          (not preview))
                 (when-let* ((text (pi-session--message-text message))
                             ((not (string-empty-p text))))
                   (setq preview text)))))))
        (dolist (entry (reverse tail-entries))
          (when (and (not name)
                     (equal (plist-get entry :type) "session_info"))
            (when-let* ((session-name (plist-get entry :name))
                        ((stringp session-name))
                        (session-name (string-trim session-name))
                        ((not (string-empty-p session-name))))
              (setq name session-name))))
        (list :path path
              :session-id session-id
              :cwd cwd
              :name name
              :preview preview
              :modified modified))
    (error nil)))

(defun pi-session--scope-name (root)
  (let ((name (file-name-nondirectory root)))
    (if (string-empty-p name) root name)))

(defun pi-session--scope-for-directory (directory)
  (let* ((dir (pi-session--normalize-dir directory))
         (git-root-raw (locate-dominating-file dir ".git"))
         (git-root (and git-root-raw (pi-session--normalize-dir git-root-raw)))
         (kind (if git-root 'project 'directory))
         (root (or git-root dir)))
    (list :kind kind
          :root root
          :name (pi-session--scope-name root)
          :key (format "%s:%s" kind root))))

(defun pi-session-scope-for-buffer (&optional buffer)
  "Return canonical scope plist for BUFFER.
The plist keys are :kind, :root, :name, and :key."
  (with-current-buffer (or buffer (current-buffer))
    (pi-session--scope-for-directory default-directory)))

(defun pi-session--lookup-scope (scope)
  (gethash (plist-get scope :key) pi-session--by-scope))

(defun pi-session-current-for-buffer (&optional buffer)
  "Return existing session for BUFFER scope, or nil."
  (pi-session--lookup-scope (pi-session-scope-for-buffer buffer)))

(defun pi-session-saved-sessions-for-buffer (&optional buffer)
  "Return saved session metadata for BUFFER scope, newest first."
  (let* ((scope (pi-session-scope-for-buffer buffer))
         (scope-key (plist-get scope :key))
         (session-dir (pi-session--session-dir-for-root (plist-get scope :root)))
         (active-file (pi-session--get-active-file scope-key))
         sessions)
    (when session-dir
      (dolist (path (directory-files session-dir t "\\.jsonl\\'"))
        (when-let* ((info (pi-session--saved-session-info path)))
          (push (plist-put info :active (equal path active-file)) sessions)))
      (setq sessions
            (sort sessions
                  (lambda (a b)
                    (time-less-p (plist-get b :modified)
                                 (plist-get a :modified))))))
    sessions))

(defun pi-session--register (session)
  (puthash (pi-session-scope-key session) session pi-session--by-scope)
  (setq pi-session--known-sessions
        (cons session (seq-remove (lambda (it)
                                    (equal (pi-session-id it)
                                           (pi-session-id session)))
                                  pi-session--known-sessions)))
  session)

(defun pi-session--cancel-idle-timer (session)
  (when-let* ((timer (pi-session-idle-timer session)))
    (cancel-timer timer)
    (setf (pi-session-idle-timer session) nil)))

(defun pi-session--streaming-p (session)
  (plist-get (pi-session-cached-state session) :is-streaming))

(defun pi-session--set-streaming (session value)
  (setf (pi-session-cached-state session)
        (plist-put (pi-session-cached-state session) :is-streaming value)))

(defun pi-session--should-keepalive-p (session)
  (and pi-session-keepalive-predicate
       (funcall pi-session-keepalive-predicate session)))

(defun pi-session--touch (session)
  (setf (pi-session-last-used-at session) (float-time))
  (pi-session--cancel-idle-timer session)
  (setf (pi-session-idle-timer session)
        (run-at-time
         pi-session-idle-timeout-seconds nil
         (lambda ()
           (when (eq (pi-session-status session) 'ready)
             (cond
              ((pi-session--streaming-p session) nil)
              ((pi-session--should-keepalive-p session)
               (pi-session--touch session))
              (t
               (pi-session-kill session t))))))))

(defun pi-session--event-dispatch (session event)
  (pcase (plist-get event :type)
    ("agent_start" (pi-session--set-streaming session t))
    ("agent_end" (pi-session--set-streaming session nil))
    (_ nil))
  (pi-session--touch session)
  (run-hook-with-args 'pi-session-event-hook session event))

(defun pi-session--clear-ready-callbacks (session)
  (setf (pi-session-ready-callbacks session) nil))

(defun pi-session--run-ready-callbacks (session)
  (let ((callbacks (pi-session-ready-callbacks session)))
    (pi-session--clear-ready-callbacks session)
    (dolist (callback callbacks)
      (funcall callback session))))

(defun pi-session--state-from-response (response)
  (let ((data (plist-get response :data)))
    (list :model (plist-get data :model)
          :thinking-level (plist-get data :thinkingLevel)
          :is-streaming (plist-get data :isStreaming)
          :is-compacting (plist-get data :isCompacting)
          :steering-mode (plist-get data :steeringMode)
          :follow-up-mode (plist-get data :followUpMode)
          :session-file (plist-get data :sessionFile)
          :session-id (plist-get data :sessionId)
          :session-name (plist-get data :sessionName)
          :auto-compaction-enabled (plist-get data :autoCompactionEnabled)
          :message-count (plist-get data :messageCount)
          :pending-message-count (plist-get data :pendingMessageCount)
          :last-refresh-at (float-time))))

(defun pi-session--apply-state (session response)
  (let ((state (pi-session--state-from-response response)))
    (setf (pi-session-cached-state session) state
          (pi-session-session-file session) (plist-get state :session-file)
          (pi-session-session-id session) (plist-get state :session-id))
    (when-let* ((name (plist-get state :session-name)))
      (setf (pi-session-name session) name))
    (when-let* ((session-file (plist-get state :session-file))
                ((stringp session-file))
                (session-file (string-trim session-file))
                ((not (string-empty-p session-file))))
      (pi-session--set-active-file (pi-session-scope-key session) session-file))
    state))

(defun pi-session-display-name (session)
  "Return a user-facing display name for SESSION."
  (or (and (stringp (pi-session-name session))
           (not (string-empty-p (pi-session-name session)))
           (pi-session-name session))
      (pi-session--scope-name (pi-session-root session))))

(defun pi-session--rpc-name (session)
  (format "%s:%s"
          (symbol-name (pi-session-scope session))
          (pi-session-display-name session)))

(defun pi-session--emit-start-error (session message)
  (setf (pi-session-status session) 'dead)
  (pi-session--clear-ready-callbacks session)
  (run-hook-with-args 'pi-session-event-hook session
                      (list :type "session_error"
                            :error message)))

(defun pi-session--exit-handler (session _rpc event)
  (pi-session--set-streaming session nil)
  (pi-session--clear-ready-callbacks session)
  (setf (pi-session-status session) 'dead
        (pi-session-rpc session) nil)
  (run-hook-with-args 'pi-session-event-hook session
                      (list :type "session_exit"
                            :event event)))

(defun pi-session--spawn (session)
  (let* ((root (file-name-as-directory (pi-session-root session)))
         (rpc (pi-rpc-start root (pi-session--rpc-name session)
                            (lambda (_rpc event)
                              (pi-session--event-dispatch session event))
                            (lambda (rpc event)
                              (pi-session--exit-handler session rpc event)))))
    (setf (pi-session-rpc session) rpc
          (pi-session-status session) 'starting)
    rpc))

(defun pi-session--request-state (session callback)
  (pi-rpc-send
   (pi-session-rpc session)
   '(("type" . "get_state"))
   (lambda (response)
     (if (eq (plist-get response :success) :json-false)
         (funcall callback session nil response)
       (pi-session--apply-state session response)
       (funcall callback session (pi-session-cached-state session) response)))))

(defun pi-session--mark-ready (session)
  (setf (pi-session-status session) 'ready)
  (pi-session--touch session)
  (pi-session--run-ready-callbacks session))

(defun pi-session--bootstrap-after-spawn (session)
  (let ((resume-file (pi-session-session-file session)))
    (pi-session--request-state
     session
     (lambda (_session state response)
       (if (eq (plist-get response :success) :json-false)
           (pi-session--emit-start-error session (or (plist-get response :error)
                                                     "Failed to read session state"))
         (let ((current-file (plist-get state :session-file)))
           (if (and resume-file
                    (not (string-empty-p resume-file))
                    (not (equal current-file resume-file)))
               (pi-rpc-send
                (pi-session-rpc session)
                `(("type" . "switch_session")
                  ("sessionPath" . ,resume-file))
                (lambda (switch-response)
                  (if (eq (plist-get switch-response :success) :json-false)
                      (pi-session--emit-start-error
                       session
                       (or (plist-get switch-response :error)
                           "Failed to switch session"))
                    (pi-session--request-state
                     session
                     (lambda (_s _state2 response2)
                       (if (eq (plist-get response2 :success) :json-false)
                           (pi-session--emit-start-error
                            session
                            (or (plist-get response2 :error)
                                "Failed to refresh resumed session state"))
                         (pi-session--mark-ready session)))))))
             (pi-session--mark-ready session))))))))

(defun pi-session--ensure-running (session callback)
  (cond
   ((and (pi-session-rpc session)
         (pi-rpc-live-p (pi-session-rpc session))
         (eq (pi-session-status session) 'ready))
    (funcall callback session))
   ((eq (pi-session-status session) 'starting)
    (setf (pi-session-ready-callbacks session)
          (append (pi-session-ready-callbacks session)
                  (list callback))))
   (t
    (setf (pi-session-ready-callbacks session)
          (append (pi-session-ready-callbacks session)
                  (list callback)))
    (pi-session--spawn session)
    (pi-session--bootstrap-after-spawn session))))

(defun pi-session--create-for-scope (scope &optional session-file)
  (let ((session (pi-session--create
                  :id (pi-session--next-id)
                  :name nil
                  :scope (plist-get scope :kind)
                  :scope-key (plist-get scope :key)
                  :root (plist-get scope :root)
                  :session-file (or session-file
                                    (pi-session--get-active-file
                                     (plist-get scope :key)))
                  :cached-state nil
                  :ready-callbacks nil
                  :status 'stopped)))
    (pi-session--register session)
    (pi-session--ensure-running session (lambda (_running) nil))
    session))

(defun pi-session-ensure-for-buffer (&optional buffer session-file)
  "Return the scope-bound session for BUFFER, creating it if needed.

When SESSION-FILE is non-nil and a new session object must be created, use it
as the initial session file to resume."
  (let* ((scope (pi-session-scope-for-buffer buffer))
         (session (pi-session--lookup-scope scope)))
    (if session
        (progn
          (when session-file
            (setf (pi-session-session-file session) session-file))
          (unless (and (pi-session-rpc session)
                       (pi-rpc-live-p (pi-session-rpc session))
                       (eq (pi-session-status session) 'ready))
            (pi-session--ensure-running session (lambda (_s) nil)))
          session)
      (pi-session--create-for-scope scope session-file))))

(defun pi-session-send-prompt (session message &optional callback)
  "Send MESSAGE through SESSION.
CALLBACK receives `(SESSION RESPONSE)'."
  (unless session
    (user-error "No pi session selected"))
  (pi-session--ensure-running
   session
   (lambda (_session)
     (pi-session--touch session)
     (let ((command (if (pi-session--streaming-p session)
                        `(("type" . "prompt")
                          ("message" . ,message)
                          ("streamingBehavior" . "followUp"))
                      `(("type" . "prompt")
                        ("message" . ,message)))))
       (pi-rpc-send
        (pi-session-rpc session)
        command
        (lambda (response)
          (when (eq (plist-get response :success) :json-false)
            (run-hook-with-args 'pi-session-event-hook session
                                (list :type "session_error"
                                      :error (plist-get response :error))))
          (when callback
            (funcall callback session response))))))))

(defun pi-session--normalize-extension-ui-payload (payload)
  "Normalize PAYLOAD for an extension UI response."
  (cond
   ((null payload) nil)
   ((and (listp payload) (keywordp (car payload)))
    (let (result)
      (while payload
        (let ((raw-key (pop payload))
              (raw-val (pop payload)))
          (push (cons (substring (symbol-name raw-key) 1) raw-val) result)))
      (nreverse result)))
   ((and (listp payload)
         (or (null payload)
             (consp (car payload))))
    payload)
   (t
    (user-error "Unsupported extension UI payload: %S" payload))))

(defun pi-session-send-extension-ui-response (session request-id payload)
  "Reply to extension UI REQUEST-ID in SESSION using PAYLOAD."
  (unless session
    (user-error "No pi session selected"))
  (unless (and (pi-session-rpc session)
               (pi-rpc-live-p (pi-session-rpc session)))
    (user-error "Pi session is not running"))
  (let ((normalized-payload (pi-session--normalize-extension-ui-payload payload)))
    (pi-session--touch session)
    (pi-rpc-notify
     (pi-session-rpc session)
     (append `(("type" . "extension_ui_response")
               ("id" . ,request-id))
             normalized-payload))))

(defun pi-session-abort (session &optional callback)
  "Abort the current run in SESSION."
  (unless session
    (user-error "No active pi session"))
  (pi-session--ensure-running
   session
   (lambda (_session)
     (pi-rpc-send
      (pi-session-rpc session)
      '(("type" . "abort"))
      (lambda (response)
        (when callback
          (funcall callback session response)))))))

(defun pi-session-load-messages (session callback)
  "Load all messages for SESSION and invoke CALLBACK.
CALLBACK receives `(SESSION RESPONSE)'."
  (pi-session--ensure-running
   session
   (lambda (_session)
     (pi-rpc-send
      (pi-session-rpc session)
      '(("type" . "get_messages"))
      (lambda (response)
        (when callback
          (funcall callback session response)))))))

(defun pi-session-get-commands (session callback)
  "Load available slash commands for SESSION and invoke CALLBACK.
CALLBACK receives `(SESSION RESPONSE)'."
  (pi-session--ensure-running
   session
   (lambda (_session)
     (pi-rpc-send
      (pi-session-rpc session)
      '(("type" . "get_commands"))
      (lambda (response)
        (when callback
          (funcall callback session response)))))))

(defun pi-session-new-session (session &optional callback)
  "Start a fresh session in SESSION scope."
  (pi-session--ensure-running
   session
   (lambda (_session)
     (pi-rpc-send
      (pi-session-rpc session)
      '(("type" . "new_session"))
      (lambda (response)
        (if (eq (plist-get response :success) :json-false)
            (when callback
              (funcall callback session response))
          (pi-session--request-state
           session
           (lambda (_s _state state-response)
             (when callback
               (funcall callback session
                        (if (eq (plist-get state-response :success) :json-false)
                            state-response
                          response)))))))))))

(defun pi-session-resume (session session-file &optional callback)
  "Switch SESSION to SESSION-FILE and remember it as the active scope session."
  (unless session
    (user-error "No pi session selected"))
  (let ((session-file (and session-file (expand-file-name session-file))))
    (if (not (and (stringp session-file)
                  (file-exists-p session-file)))
        (when callback
          (funcall callback
                   session
                   (list :success :json-false
                         :error (format "Session file does not exist: %s"
                                        (or session-file "")))))
      (setf (pi-session-session-file session) session-file)
      (pi-session--ensure-running
       session
       (lambda (_session)
         (pi-session--request-state
          session
          (lambda (_s state state-response)
            (if (eq (plist-get state-response :success) :json-false)
                (when callback
                  (funcall callback session state-response))
              (let ((current-file (plist-get state :session-file)))
                (if (equal current-file session-file)
                    (progn
                      (pi-session--set-active-file (pi-session-scope-key session)
                                                   session-file)
                      (when callback
                        (funcall callback session
                                 (list :success t
                                       :data (pi-session-cached-state session)))))
                  (pi-rpc-send
                   (pi-session-rpc session)
                   `(("type" . "switch_session")
                     ("sessionPath" . ,session-file))
                   (lambda (switch-response)
                     (if (eq (plist-get switch-response :success) :json-false)
                         (when callback
                           (funcall callback session switch-response))
                       (pi-session--request-state
                        session
                        (lambda (_s2 _state2 response2)
                          (unless (eq (plist-get response2 :success) :json-false)
                            (pi-session--set-active-file (pi-session-scope-key session)
                                                         session-file))
                          (when callback
                            (funcall callback session
                                     (if (eq (plist-get response2 :success) :json-false)
                                         response2
                                       (list :success t
                                             :data (pi-session-cached-state session))))))))))))))))))))

(defun pi-session-get-available-models (session callback)
  "Load configured models for SESSION and invoke CALLBACK.
CALLBACK receives `(SESSION RESPONSE)'."
  (pi-session--ensure-running
   session
   (lambda (_session)
     (pi-rpc-send
      (pi-session-rpc session)
      '(("type" . "get_available_models"))
      (lambda (response)
        (when callback
          (funcall callback session response)))))))

(defun pi-session-compact (session &optional custom-instructions callback)
  "Compact SESSION context with optional CUSTOM-INSTRUCTIONS."
  (pi-session--ensure-running
   session
   (lambda (_session)
     (let ((command (append '(("type" . "compact"))
                            (when (and (stringp custom-instructions)
                                       (not (string-empty-p custom-instructions)))
                              `(("customInstructions" . ,custom-instructions))))))
       (pi-rpc-send
        (pi-session-rpc session)
        command
        (lambda (response)
          (when callback
            (funcall callback session response))))))))

(defun pi-session-export-html (session &optional output-path callback)
  "Export SESSION to HTML, optionally writing to OUTPUT-PATH."
  (pi-session--ensure-running
   session
   (lambda (_session)
     (let ((command (append '(("type" . "export_html"))
                            (when (and (stringp output-path)
                                       (not (string-empty-p output-path)))
                              `(("outputPath" . ,output-path))))))
       (pi-rpc-send
        (pi-session-rpc session)
        command
        (lambda (response)
          (when callback
            (funcall callback session response))))))))

(defun pi-session-set-model (session provider model-id &optional callback)
  "Switch SESSION to PROVIDER/MODEL-ID."
  (pi-session--ensure-running
   session
   (lambda (_session)
     (pi-rpc-send
      (pi-session-rpc session)
      `(("type" . "set_model")
        ("provider" . ,provider)
        ("modelId" . ,model-id))
      (lambda (response)
        (if (eq (plist-get response :success) :json-false)
            (when callback
              (funcall callback session response))
          (pi-session--request-state
           session
           (lambda (_s _state _state-response)
             (when callback
               (funcall callback session response))))))))))

(defun pi-session-cycle-model (session &optional callback)
  "Cycle to the next model in SESSION."
  (pi-session--ensure-running
   session
   (lambda (_session)
     (pi-rpc-send
      (pi-session-rpc session)
      '(("type" . "cycle_model"))
      (lambda (response)
        (unless (eq (plist-get response :success) :json-false)
          (when-let* ((data (plist-get response :data))
                      (model (plist-get data :model)))
            (setf (pi-session-cached-state session)
                  (plist-put (pi-session-cached-state session)
                             :model
                             model)))
          (when-let* ((data (plist-get response :data))
                      (thinking-level (plist-get data :thinkingLevel)))
            (setf (pi-session-cached-state session)
                  (plist-put (pi-session-cached-state session)
                             :thinking-level
                             thinking-level))))
        (when callback
          (funcall callback session response)))))))

(defun pi-session-set-thinking-level (session level &optional callback)
  "Set SESSION reasoning LEVEL."
  (pi-session--ensure-running
   session
   (lambda (_session)
     (pi-rpc-send
      (pi-session-rpc session)
      `(("type" . "set_thinking_level")
        ("level" . ,level))
      (lambda (response)
        (if (eq (plist-get response :success) :json-false)
            (when callback
              (funcall callback session response))
          (pi-session--request-state
           session
           (lambda (_s _state _state-response)
             (when callback
               (funcall callback session response))))))))))

(defun pi-session-cycle-thinking-level (session &optional callback)
  "Cycle to the next reasoning level for SESSION."
  (pi-session--ensure-running
   session
   (lambda (_session)
     (pi-rpc-send
      (pi-session-rpc session)
      '(("type" . "cycle_thinking_level"))
      (lambda (response)
        (unless (eq (plist-get response :success) :json-false)
          (when-let* ((data (plist-get response :data))
                      (level (plist-get data :level)))
            (setf (pi-session-cached-state session)
                  (plist-put (pi-session-cached-state session)
                             :thinking-level
                             level))))
        (when callback
          (funcall callback session response)))))))

(defun pi-session-restart (session)
  "Restart SESSION process and preserve session context."
  (unless session
    (user-error "No pi session selected"))
  (pi-session-kill session t)
  (setf (pi-session-status session) 'stopped)
  (pi-session--ensure-running
   session
   (lambda (_session)
     (message "Restarted pi session: %s" (pi-session-display-name session))))
  session)

(defun pi-session-kill (session &optional silent)
  "Kill SESSION subprocess.
When SILENT is non-nil, do not show a message."
  (unless session
    (user-error "No pi session selected"))
  (pi-session--cancel-idle-timer session)
  (pi-session--clear-ready-callbacks session)
  (when-let* ((rpc (pi-session-rpc session)))
    (when (pi-rpc-live-p rpc)
      (pi-rpc-stop rpc)))
  (setf (pi-session-status session) 'stopped
        (pi-session-rpc session) nil)
  (unless silent
    (message "Killed pi session: %s" (pi-session-display-name session)))
  session)

(defun pi-session-list ()
  "Return in-memory session summaries."
  (mapcar
   (lambda (session)
     (list :id (pi-session-id session)
           :name (pi-session-display-name session)
           :scope (pi-session-scope session)
           :scope-key (pi-session-scope-key session)
           :root (pi-session-root session)
           :status (pi-session-status session)
           :session-file (pi-session-session-file session)
           :session-id (pi-session-session-id session)))
   (sort (copy-sequence pi-session--known-sessions)
         (lambda (a b)
           (string-lessp (pi-session-scope-key a)
                         (pi-session-scope-key b))))))

(defun pi-session--format-summary (summary)
  (format "%s (%s, %s) — %s"
          (or (plist-get summary :name) "unnamed")
          (plist-get summary :scope)
          (plist-get summary :status)
          (or (plist-get summary :root) "?")))

(defun pi-list-sessions ()
  "Display in-memory pi sessions."
  (interactive)
  (with-output-to-temp-buffer "*pi-sessions*"
    (princ "Pi sessions\n\n")
    (dolist (summary (pi-session-list))
      (princ (pi-session--format-summary summary))
      (when-let* ((file (plist-get summary :session-file)))
        (princ (format "\n  file: %s" file)))
      (when-let* ((sid (plist-get summary :session-id)))
        (princ (format "\n  id: %s" sid)))
      (princ "\n\n"))))


(provide 'pi-session)

;;; pi-session.el ends here
