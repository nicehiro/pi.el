;;; pi-extension-ui.el --- Extension UI request handling for pi -*- lexical-binding: t; -*-

;; Author: Fangyuan and contributors
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Interactive handling for extension UI requests emitted by pi.

;;; Code:

(require 'subr-x)
(require 'pi-session)

(declare-function pi-ui--append-transient "pi-session-buffer" (buffer item))
(declare-function pi-ui--display-prompt-buffer "pi-prompt" (buffer))

(defvar pi-ui--extension-ui-request-queue nil)
(defvar pi-ui--extension-ui-request-active nil)

(defvar-local pi-ui--extension-editor-session nil)
(defvar-local pi-ui--extension-editor-request-id nil)
(defvar-local pi-ui--extension-editor-finished nil)

(defvar pi-ui-extension-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pi-ui-extension-editor-submit)
    (define-key map (kbd "C-c C-k") #'pi-ui-extension-editor-cancel)
    map)
  "Keymap for `pi-ui-extension-editor-mode'.")

(define-derived-mode pi-ui-extension-editor-mode text-mode "Pi-Extension-Editor"
  "Major mode for extension editor requests."
  (setq-local header-line-format
              "Pi editor: C-c C-c submit • C-c C-k cancel")
  (setq-local pi-ui--extension-editor-session nil)
  (setq-local pi-ui--extension-editor-request-id nil)
  (setq-local pi-ui--extension-editor-finished nil)
  (add-hook 'kill-buffer-hook #'pi-ui--extension-editor-buffer-killed nil t))

(defun pi-ui--finish-extension-ui-request ()
  (setq pi-ui--extension-ui-request-active nil)
  (when pi-ui--extension-ui-request-queue
    (run-at-time 0 nil #'pi-ui--start-next-extension-ui-request)))

(defun pi-ui--send-extension-ui-response (session request-id payload)
  (condition-case err
      (pi-session-send-extension-ui-response session request-id payload)
    (error
     (message "pi extension UI response failed: %s" (error-message-string err)))))

(defun pi-ui--extension-ui-request-note (event)
  (pcase (plist-get event :method)
    ("select"
     (format "Selection requested: %s" (or (plist-get event :title) "Choose an option")))
    ("confirm"
     (format "Confirmation requested: %s" (or (plist-get event :title) "Confirm")))
    ("input"
     (format "Input requested: %s" (or (plist-get event :title) "Enter a value")))
    ("editor"
     (format "Editor requested: %s" (or (plist-get event :title) "Edit text")))
    ("setStatus"
     (format "Status %s: %s"
             (or (plist-get event :statusKey) "")
             (or (plist-get event :statusText) "")))
    ("setWidget"
     (let ((lines (plist-get event :widgetLines)))
       (format "Widget %s%s"
               (or (plist-get event :widgetKey) "")
               (if (and (listp lines) lines)
                   (concat "\n" (string-join lines "\n"))
                 ""))))
    ("setTitle"
     (format "Title set to: %s" (or (plist-get event :title) "")))
    ("set_editor_text"
     "Extension requested editor text update")
    (_ nil)))

(defun pi-ui--enqueue-extension-ui-request (session buffer event)
  (when-let* ((note (pi-ui--extension-ui-request-note event))
              ((buffer-live-p buffer)))
    (pi-ui--append-transient buffer (list :kind 'extension :text note)))
  (setq pi-ui--extension-ui-request-queue
        (append pi-ui--extension-ui-request-queue
                (list (list :session session :buffer buffer :event event))))
  (unless pi-ui--extension-ui-request-active
    (run-at-time 0 nil #'pi-ui--start-next-extension-ui-request)))

(defun pi-ui--start-next-extension-ui-request ()
  (unless pi-ui--extension-ui-request-active
    (when-let* ((request (car pi-ui--extension-ui-request-queue)))
      (setq pi-ui--extension-ui-request-queue (cdr pi-ui--extension-ui-request-queue)
            pi-ui--extension-ui-request-active request)
      (run-at-time 0 nil #'pi-ui--process-extension-ui-request request))))

(defun pi-ui--extension-ui-select (title options)
  (let ((choice (completing-read (format "%s: " title) options nil t)))
    (unless (string-empty-p choice)
      choice)))

(defun pi-ui--process-extension-ui-request (request)
  (let* ((session (plist-get request :session))
         (event (plist-get request :event))
         (request-id (plist-get event :id))
         (method (plist-get event :method))
         (title (or (plist-get event :title) "Pi")))
    (pcase method
      ("editor"
       (pi-ui--show-extension-editor request))
      (_
       (unwind-protect
           (condition-case nil
               (pcase method
                 ("select"
                  (if-let* ((choice (pi-ui--extension-ui-select
                                     title (or (plist-get event :options) '()))))
                      (pi-ui--send-extension-ui-response
                       session request-id `(("value" . ,choice)))
                    (pi-ui--send-extension-ui-response
                     session request-id '(("cancelled" . t)))))
                 ("confirm"
                  (let ((confirmed
                         (y-or-n-p
                          (string-trim
                           (format "%s%s "
                                   title
                                   (if-let* ((message (plist-get event :message)))
                                       (format " — %s" message)
                                     ""))))))
                    (pi-ui--send-extension-ui-response
                     session request-id `(("confirmed" . ,confirmed)))))
                 ("input"
                  (let ((value (read-string (format "%s: " title)
                                            nil nil
                                            (or (plist-get event :placeholder) ""))))
                    (pi-ui--send-extension-ui-response
                     session request-id `(("value" . ,value)))))
                 (_
                  (pi-ui--send-extension-ui-response
                   session request-id '(("cancelled" . t)))))
             (quit
              (pi-ui--send-extension-ui-response
               session request-id '(("cancelled" . t))))
             (error
              (pi-ui--send-extension-ui-response
               session request-id '(("cancelled" . t)))))
         (pi-ui--finish-extension-ui-request))))))

(defun pi-ui--extension-editor-finalize (buffer payload)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless pi-ui--extension-editor-finished
        (let ((session pi-ui--extension-editor-session)
              (request-id pi-ui--extension-editor-request-id))
          (setq-local pi-ui--extension-editor-finished t)
          (pi-ui--send-extension-ui-response session request-id payload))))
    (pi-ui--finish-extension-ui-request)))

(defun pi-ui-extension-editor-submit ()
  "Submit the current extension editor buffer."
  (interactive)
  (let ((buffer (current-buffer))
        (window (selected-window))
        (text (buffer-substring-no-properties (point-min) (point-max))))
    (pi-ui--extension-editor-finalize buffer `(("value" . ,text)))
    (when (window-live-p window)
      (quit-window t window))))

(defun pi-ui-extension-editor-cancel ()
  "Cancel the current extension editor buffer."
  (interactive)
  (let ((buffer (current-buffer))
        (window (selected-window)))
    (pi-ui--extension-editor-finalize buffer '(("cancelled" . t)))
    (when (window-live-p window)
      (quit-window t window))))

(defun pi-ui--extension-editor-buffer-killed ()
  (unless pi-ui--extension-editor-finished
    (pi-ui--extension-editor-finalize (current-buffer) '(("cancelled" . t)))))

(defun pi-ui--show-extension-editor (request)
  (let* ((session (plist-get request :session))
         (event (plist-get request :event))
         (title (or (plist-get event :title) "Pi editor"))
         (prefill (or (plist-get event :prefill) ""))
         (buffer (generate-new-buffer (format "*pi-editor:%s*" title))))
    (with-current-buffer buffer
      (pi-ui-extension-editor-mode)
      (setq-local pi-ui--extension-editor-session session)
      (setq-local pi-ui--extension-editor-request-id (plist-get event :id))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert prefill)
        (goto-char (point-min))))
    (let ((window (pi-ui--display-prompt-buffer buffer)))
      (when (window-live-p window)
        (select-window window)))
    buffer))

(defun pi-ui--handle-extension-ui-event (session buffer event)
  (pcase (plist-get event :method)
    ("notify"
     (pi-ui--append-transient
      buffer
      (list :kind 'notify :text (or (plist-get event :message) ""))))
    ((or "select" "confirm" "input" "editor")
     (pi-ui--enqueue-extension-ui-request session buffer event))
    ((or "setStatus" "setWidget" "setTitle" "set_editor_text")
     (when-let* ((note (pi-ui--extension-ui-request-note event)))
       (pi-ui--append-transient buffer (list :kind 'extension :text note))))
    (_ nil)))

(provide 'pi-extension-ui)

;;; pi-extension-ui.el ends here
