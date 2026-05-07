;;; pi-session-buffer.el --- Session buffer UI for pi -*- lexical-binding: t; -*-

;; Author: Fangyuan and contributors
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Session buffer management, rendering, and event handling for pi.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pi-rpc)
(require 'pi-session)
(require 'pi-prompt)
(require 'pi-render)
(require 'pi-extension-ui)
(require 'markdown-mode nil t)

(declare-function pi-rpc-success-p "pi-rpc" (response))
(declare-function pi-session-buffer-mode "pi-session-buffer" ())
(declare-function pi-steer "pi" (message))
(declare-function pi-follow-up "pi" (message))
(declare-function pi-toggle-auto-compaction "pi" ())
(declare-function pi-toggle-auto-retry "pi" ())
(declare-function pi-abort-retry "pi" ())

(defvar pi-ui-window-side)
(defvar pi-ui-window-size)
(defvar pi-ui-auto-scroll)
(defvar pi-ui-enable-streaming)
(defvar pi-ui-follow-current-buffer)
(defvar pi-ui-stream-render-interval)
(defvar pi-ui-tool-display-style)

(defvar pi-ui-session-title-face)
(defvar pi-ui-meta-face)
(defvar pi-ui-placeholder-face)

(defvar pi-ui--session-buffers (make-hash-table :test #'equal))
(defvar pi-ui--follow-current-buffer-last nil)

(defvar-local pi-ui--session nil)
(defvar-local pi-ui--source-buffer nil)
(defvar-local pi-ui--history nil)
(defvar-local pi-ui--live-message nil)
(defvar-local pi-ui--transient-items nil)
(defvar-local pi-ui--loading nil)
(defvar-local pi-ui--pending-render-timer nil)
(defvar-local pi-ui--tool-call-summary-by-id nil)
(defvar-local pi-ui--extension-status nil)
(defvar-local pi-ui--extension-widgets nil)
(defvar-local pi-ui--extension-title nil)

(defvar pi-session-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g")
                (lambda ()
                  (interactive)
                  (pi-ui--session-buffer-refresh)))
    (define-key map (kbd "b")
                (lambda ()
                  (interactive)
                  (pi-ui--session-buffer-jump-to-source)))
    (define-key map (kbd "a")
                (lambda ()
                  (interactive)
                  (pi-ui--session-buffer-abort)))
    (define-key map (kbd "s") #'pi-ui--session-buffer-compose-prompt)
    (define-key map (kbd "S") #'pi-steer)
    (define-key map (kbd "F") #'pi-follow-up)
    (define-key map (kbd "A") #'pi-toggle-auto-compaction)
    (define-key map (kbd "M-r") #'pi-toggle-auto-retry)
    (define-key map (kbd "M-a") #'pi-abort-retry)
    (define-key map (kbd "c")
                (lambda ()
                  (interactive)
                  (pi-ui--session-buffer-clear-and-reload)))
    (define-key map (kbd "t") #'pi-ui-toggle-tool-display-style)
    (define-key map (kbd "f") #'pi-ui-toggle-auto-scroll)
    map)
  "Keymap for `pi-session-buffer-mode'.")

(defun pi-ui--init-session-buffer-state ()
  (setq buffer-read-only t)
  (setq-local truncate-lines nil)
  (setq-local pi-ui--session nil)
  (setq-local pi-ui--source-buffer nil)
  (setq-local pi-ui--history nil)
  (setq-local pi-ui--live-message nil)
  (setq-local pi-ui--transient-items nil)
  (setq-local pi-ui--loading nil)
  (setq-local pi-ui--pending-render-timer nil)
  (setq-local pi-ui--tool-call-summary-by-id nil)
  (setq-local pi-ui--extension-status nil)
  (setq-local pi-ui--extension-widgets nil)
  (setq-local pi-ui--extension-title nil)
  (setq-local header-line-format nil)
  (add-hook 'kill-buffer-hook #'pi-ui--session-buffer-killed nil t))

(if (fboundp 'markdown-mode)
    (define-derived-mode pi-session-buffer-mode markdown-mode "Pi-Session"
      "Major mode for pi session buffers."
      (pi-ui--init-session-buffer-state))
  (define-derived-mode pi-session-buffer-mode special-mode "Pi-Session"
    "Major mode for pi session buffers."
    (pi-ui--init-session-buffer-state)))

(defun pi-ui--cancel-pending-render ()
  (when (timerp pi-ui--pending-render-timer)
    (cancel-timer pi-ui--pending-render-timer))
  (setq-local pi-ui--pending-render-timer nil))

(defun pi-ui--session-buffer-killed ()
  (pi-ui--cancel-pending-render)
  (when pi-ui--session
    (let ((current (gethash (pi-session-id pi-ui--session) pi-ui--session-buffers)))
      (when (eq current (current-buffer))
        (remhash (pi-session-id pi-ui--session) pi-ui--session-buffers)))))

(defun pi-ui--session-buffer-name (session)
  (format "*pi:%s*" (pi-session-display-name session)))

(defun pi-ui--get-buffer (session)
  (gethash (pi-session-id session) pi-ui--session-buffers))

(defun pi-ui--session-visible-p (session)
  (when-let* ((buffer (pi-ui--get-buffer session)))
    (and (buffer-live-p buffer)
         (get-buffer-window buffer t))))

(defun pi-ui--register-buffer (session buffer)
  (puthash (pi-session-id session) buffer pi-ui--session-buffers)
  buffer)

(defun pi-ui--ensure-buffer (session &optional source-buffer)
  (let* ((existing (pi-ui--get-buffer session))
         (buffer (if (buffer-live-p existing)
                     existing
                   (progn
                     (when existing
                       (remhash (pi-session-id session) pi-ui--session-buffers))
                     (generate-new-buffer (pi-ui--session-buffer-name session))))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'pi-session-buffer-mode)
        (pi-session-buffer-mode))
      (setq-local pi-ui--session session)
      (when (buffer-live-p source-buffer)
        (setq-local pi-ui--source-buffer source-buffer))
      (rename-buffer (pi-ui--session-buffer-name session) t))
    (pi-ui--register-buffer session buffer)))

(defun pi-ui--display-buffer (buffer)
  (let ((window
         (display-buffer-in-side-window
          buffer
          `((side . ,pi-ui-window-side)
            ,@(if (memq pi-ui-window-side '(left right))
                  `((window-width . ,pi-ui-window-size))
                `((window-height . ,pi-ui-window-size)))))))
    (when (window-live-p window)
      (pi-ui--update-session-header-line buffer))
    window))

(defun pi-ui--session-buffer-window-p (window)
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (derived-mode-p 'pi-session-buffer-mode))))

(defun pi-ui--visible-session-windows (&optional frame)
  (seq-filter #'pi-ui--session-buffer-window-p
              (window-list frame 'no-minibuf)))

(defun pi-ui--source-buffer-p (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (not (or (minibufferp)
               (derived-mode-p 'pi-session-buffer-mode)
               (derived-mode-p 'pi-prompt-buffer-mode))))))

(defun pi-ui--follow-current-buffer ()
  (when (bound-and-true-p pi-ui-follow-current-buffer)
    (let* ((window (selected-window))
           (frame (window-frame window))
           (source-buffer (window-buffer window)))
      (when (and (pi-ui--source-buffer-p source-buffer)
                 (pi-ui--visible-session-windows frame))
        (let ((session (pi-session-current-for-buffer source-buffer)))
          (when session
            (let* ((state (cons frame session))
                   (target-buffer (pi-ui--ensure-buffer session source-buffer)))
              (unless (equal state pi-ui--follow-current-buffer-last)
                (setq pi-ui--follow-current-buffer-last state)
                (pi-ui--refresh-session-buffer session target-buffer))
              (dolist (session-window (pi-ui--visible-session-windows frame))
                (unless (eq (window-buffer session-window) target-buffer)
                  (set-window-buffer session-window target-buffer))
                (pi-ui--update-session-header-line target-buffer)))))))))

(add-hook 'post-command-hook #'pi-ui--follow-current-buffer)

(defun pi-ui--window-at-bottom-p (window)
  (with-current-buffer (window-buffer window)
    (>= (window-end window t) (max (point-min) (1- (point-max))))))

(defun pi-ui--scroll-window-to-end (window)
  (when (window-live-p window)
    (set-window-point window (point-max))
    (unless (pos-visible-in-window-p (point-max) window t)
      (set-window-start window
                        (save-excursion
                          (goto-char (point-max))
                          (line-beginning-position))
                        t))))

(defun pi-ui--schedule-render (buffer &optional force)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if force
          (progn
            (pi-ui--cancel-pending-render)
            (pi-ui--buffer-render buffer))
        (unless (timerp pi-ui--pending-render-timer)
          (setq-local
           pi-ui--pending-render-timer
           (run-at-time
            pi-ui-stream-render-interval nil
            (lambda (buf)
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (setq-local pi-ui--pending-render-timer nil))
                (pi-ui--buffer-render buf)))
            buffer)))))))

(defun pi-ui--buffer-render (buffer)
  (with-current-buffer buffer
    (let* ((inhibit-read-only t)
           (history pi-ui--history)
           (transients pi-ui--transient-items)
           (live pi-ui--live-message)
           (extension-status pi-ui--extension-status)
           (extension-widgets pi-ui--extension-widgets)
           (extension-title pi-ui--extension-title)
           (session pi-ui--session)
           (windows (get-buffer-window-list buffer nil t))
           (follow-windows (and pi-ui-auto-scroll
                                (mapcar (lambda (win)
                                          (cons win (pi-ui--window-at-bottom-p win)))
                                        windows))))
      (pi-ui--rebuild-tool-call-summary-index history live)
      (pi-ui--update-session-header-line buffer)
      (erase-buffer)
      (insert (propertize (format "pi session: %s\n" (pi-session-display-name session))
                          'face 'pi-ui-session-title-face))
      (insert (propertize (format "scope: %s\nroot: %s\n\n"
                                  (pi-session-scope session)
                                  (pi-session-root session))
                          'face 'pi-ui-meta-face))
      (when (or extension-title extension-status extension-widgets)
        (insert (pi-ui--render-extension-state
                 extension-status extension-widgets extension-title)))
      (when pi-ui--loading
        (insert (propertize "loading session messages...\n\n" 'face 'pi-ui-placeholder-face)))
      (let ((prev-tool-line nil))
        (cl-labels
            ((insert-entry (text is-tool)
               (unless (string-empty-p text)
                 (when (and prev-tool-line (not is-tool))
                   (insert "\n"))
                 (insert text)
                 (setq prev-tool-line is-tool))))
          (dolist (message history)
            (insert-entry (pi-ui--render-message message)
                          (equal (plist-get message :role) "toolResult")))
          (dolist (item transients)
            (insert-entry (pi-ui--render-transient-item item)
                          (eq (plist-get item :kind) 'tool-result)))
          (when live
            (insert-entry (pi-ui--render-message live)
                          (equal (plist-get live :role) "toolResult")))))
      (goto-char (point-max))
      (when pi-ui-auto-scroll
        (dolist (entry follow-windows)
          (pcase-let ((`(,win . ,should-follow) entry))
            (when (and should-follow (window-live-p win))
              (pi-ui--scroll-window-to-end win))))))))

(defun pi-ui--append-transient (buffer item)
  (with-current-buffer buffer
    (setq-local pi-ui--transient-items
                (append pi-ui--transient-items (list item))))
  (pi-ui--buffer-render buffer))

(defun pi-ui--set-extension-status (buffer key text)
  "Set extension status KEY to TEXT in BUFFER, or clear it when TEXT is nil."
  (when (and (buffer-live-p buffer) (stringp key) (not (string-empty-p key)))
    (with-current-buffer buffer
      (setq-local pi-ui--extension-status
                  (if text
                      (cons (cons key text) (assoc-delete-all key pi-ui--extension-status))
                    (assoc-delete-all key pi-ui--extension-status)))))
  (pi-ui--schedule-render buffer))

(defun pi-ui--set-extension-widget (buffer key lines placement &optional clear)
  "Set extension widget KEY in BUFFER, or clear it when CLEAR is non-nil."
  (when (and (buffer-live-p buffer) (stringp key) (not (string-empty-p key)))
    (with-current-buffer buffer
      (setq-local pi-ui--extension-widgets
                  (if clear
                      (assoc-delete-all key pi-ui--extension-widgets)
                    (cons (list key lines placement)
                          (assoc-delete-all key pi-ui--extension-widgets))))))
  (pi-ui--schedule-render buffer))

(defun pi-ui--set-extension-title (buffer title)
  "Set extension TITLE in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local pi-ui--extension-title title)))
  (pi-ui--schedule-render buffer))

(defun pi-ui--upsert-tool-transient (buffer item)
  "Insert or replace a live tool transient ITEM in BUFFER."
  (let ((tool-call-id (plist-get item :tool-call-id)))
    (with-current-buffer buffer
      (setq-local
       pi-ui--transient-items
       (if (and (stringp tool-call-id) (not (string-empty-p tool-call-id)))
           (let* ((replaced nil)
                  (items (mapcar (lambda (existing)
                                   (if (and (eq (plist-get existing :kind) 'tool-result)
                                            (equal (plist-get existing :tool-call-id) tool-call-id))
                                       (progn
                                         (setq replaced t)
                                         item)
                                     existing))
                                 pi-ui--transient-items)))
             (if replaced items (append items (list item))))
         (append pi-ui--transient-items (list item))))))
  (pi-ui--schedule-render buffer))

(defun pi-ui--append-history-message (buffer message)
  (with-current-buffer buffer
    (setq-local pi-ui--history
                (append pi-ui--history (list message)))
    (when (and pi-ui--live-message
               (equal (plist-get pi-ui--live-message :timestamp)
                      (plist-get message :timestamp)))
      (setq-local pi-ui--live-message nil)))
  (pi-ui--buffer-render buffer))

(defun pi-ui--set-history (buffer messages)
  (with-current-buffer buffer
    (setq-local pi-ui--history messages
                pi-ui--live-message nil
                pi-ui--transient-items nil
                pi-ui--loading nil))
  (pi-ui--buffer-render buffer))

(defun pi-ui--refresh-session-buffer (session &optional buffer)
  (let ((buffer (or buffer (pi-ui--ensure-buffer session))))
    (with-current-buffer buffer
      (setq-local pi-ui--loading t))
    (pi-ui--buffer-render buffer)
    (pi-session-load-messages
     session
     (lambda (_session response)
       (if (not (pi-rpc-success-p response))
           (progn
             (message "pi: %s" (plist-get response :error))
             (pi-ui--append-transient
              buffer
              (list :kind 'error :text (plist-get response :error))))
         (let ((messages (plist-get (plist-get response :data) :messages)))
           (pi-ui--set-history buffer messages)))))))

(defun pi-ui-show-session-buffer (&optional session source-buffer)
  "Open or show the response buffer for SESSION.
If SESSION is nil, resolve from SOURCE-BUFFER scope and create on demand."
  (let* ((source-buffer (or source-buffer (current-buffer)))
         (session (or session
                      (pi-session-ensure-for-buffer source-buffer)))
         (buffer (pi-ui--ensure-buffer session source-buffer)))
    (pi-ui--refresh-session-buffer session buffer)
    (pi-ui--display-buffer buffer)
    buffer))

(defun pi-ui-toggle-session-buffer (session &optional source-buffer)
  "Toggle the side window for SESSION without creating a session."
  (unless session
    (user-error "pi: no existing session for this project"))
  (let* ((buffer (pi-ui--get-buffer session))
         (windows (and (buffer-live-p buffer)
                       (get-buffer-window-list buffer nil (selected-frame)))))
    (if windows
        (progn
          (dolist (window windows)
            (when (window-live-p window)
              (quit-window nil window)))
          (message "pi: hid session window"))
      (setq buffer (pi-ui--ensure-buffer session source-buffer))
      (pi-ui--refresh-session-buffer session buffer)
      (pi-ui--display-buffer buffer)
      buffer)))

(defun pi-ui-open-session (&optional source-buffer)
  "Open and focus SOURCE-BUFFER's scope session buffer."
  (let* ((source-buffer (or source-buffer (current-buffer)))
         (session (pi-session-ensure-for-buffer source-buffer))
         (buffer (pi-ui-show-session-buffer session source-buffer))
         (window (get-buffer-window buffer t)))
    (when (window-live-p window)
      (select-window window))
    buffer))

(defun pi-ui-send-prompt (source-buffer prompt)
  "Send PROMPT using SOURCE-BUFFER's scope-bound session.
Show or create the session buffer, but keep focus in SOURCE-BUFFER window."
  (let* ((source-buffer (or source-buffer (current-buffer)))
         (source-window (selected-window))
         (session (pi-session-ensure-for-buffer source-buffer))
         (buffer (pi-ui-show-session-buffer session source-buffer)))
    (pi-session-send-prompt
     session prompt
     (lambda (_session response)
       (when (not (pi-rpc-success-p response))
         (message "pi: %s" (plist-get response :error)))))
    (when (window-live-p source-window)
      (select-window source-window))
    buffer))

(defun pi-ui--event-message-role (event)
  (let ((message (plist-get event :message)))
    (and message (plist-get message :role))))

(defun pi-ui--message-update-assistant-p (event)
  "Return non-nil when EVENT is a current assistant message update."
  (let ((message-event (plist-get event :assistantMessageEvent)))
    (and (listp message-event)
         (equal (pi-ui--event-message-role event) "assistant"))))

(defun pi-ui--tool-event-content-text (object)
  "Extract text content from a current RPC tool result OBJECT."
  (let ((content (and object (plist-get object :content))))
    (cond
     ((listp content)
      (string-join
       (delq nil
             (mapcar (lambda (block)
                       (let ((text (pi-ui--stringify-content-block block)))
                         (unless (string-empty-p text) text)))
                     content))
       "\n"))
     ((stringp content) content)
     (t ""))))

(defun pi-ui--format-count (value singular plural)
  (when (numberp value)
    (format "%s %s" value (if (= value 1) singular plural))))

(defun pi-ui--format-delay-ms (delay-ms)
  (when (numberp delay-ms)
    (format "%.1fs" (/ delay-ms 1000.0))))

(defun pi-ui--compaction-result-summary (result)
  (let ((parts nil))
    (when-let* ((tokens (plist-get result :tokensBefore)))
      (push (pi-ui--format-count tokens "token" "tokens") parts))
    (when-let* ((entry-id (plist-get result :firstKeptEntryId))
                ((stringp entry-id))
                ((not (string-empty-p entry-id))))
      (push (format "kept from %s" (substring entry-id 0 (min 8 (length entry-id)))) parts))
    (string-join (nreverse (delq nil parts)) ", ")))

(defun pi-ui--status-item-from-event (event)
  (pcase (plist-get event :type)
    ("compaction_start"
     (list :kind 'status
           :text (format "Compaction started%s"
                         (if-let* ((reason (plist-get event :reason)))
                             (format " (%s)" reason)
                           ""))))
    ("compaction_end"
     (let ((reason (plist-get event :reason))
           (result (plist-get event :result))
           (error (plist-get event :errorMessage)))
       (list :kind 'status
             :text (cond
                    ((eq (plist-get event :aborted) t)
                     (format "Compaction cancelled%s"
                             (if reason (format " (%s)" reason) "")))
                    (error
                     (format "Compaction failed%s: %s"
                             (if reason (format " (%s)" reason) "")
                             error))
                    (result
                     (let ((summary (pi-ui--compaction-result-summary result)))
                       (format "Compaction complete%s%s%s"
                               (if reason (format " (%s)" reason) "")
                               (if (string-empty-p summary) "" (format ": %s" summary))
                               (if (eq (plist-get event :willRetry) t)
                                   "; retrying prompt"
                                 ""))))
                    (t "Compaction complete")))))
    ("auto_retry_start"
     (list :kind 'status
           :text (format "Auto-retry %s/%s in %s%s"
                         (or (plist-get event :attempt) "?")
                         (or (plist-get event :maxAttempts) "?")
                         (or (pi-ui--format-delay-ms (plist-get event :delayMs)) "soon")
                         (if-let* ((error (plist-get event :errorMessage)))
                             (format ": %s" (pi-ui--truncate-inline error 120))
                           ""))))
    ("auto_retry_end"
     (list :kind 'status
           :text (if (eq (plist-get event :success) t)
                     (format "Auto-retry succeeded%s"
                             (if-let* ((attempt (plist-get event :attempt)))
                                 (format " after attempt %s" attempt)
                               ""))
                   (format "Auto-retry stopped%s%s"
                           (if-let* ((attempt (plist-get event :attempt)))
                               (format " after %s attempt%s" attempt (if (= attempt 1) "" "s"))
                             "")
                           (if-let* ((error (plist-get event :finalError)))
                               (format ": %s" (pi-ui--truncate-inline error 120))
                             "")))))))

(defun pi-ui--tool-result-from-event (event)
  (let* ((result (or (plist-get event :result)
                     (plist-get event :partialResult)))
         (tool-name (or (plist-get event :toolName)
                        (and result (plist-get result :toolName))))
         (tool-call-id (or (plist-get event :toolCallId)
                           (and result (plist-get result :toolCallId))))
         (arguments (or (plist-get event :args)
                        (and result (plist-get result :args))
                        (pi-ui--tool-arguments-from-object event)
                        (and result (pi-ui--tool-arguments-from-object result))))
         (event-type (plist-get event :type))
         (status (pcase event-type
                   ("tool_execution_start" 'running)
                   ("tool_execution_update" 'running)
                   (_ (pi-ui--tool-status-from-event event)))))
    (list :kind 'tool-result
          :tool-name tool-name
          :tool-call-id tool-call-id
          :arguments arguments
          :detail (pi-ui--tool-detail-from-arguments tool-name arguments)
          :status status
          :duration-ms (pi-ui--tool-duration-from-event event)
          :text (pi-ui--tool-event-content-text result))))

(defun pi-ui--handle-session-event (session event)
  (let* ((event-type (plist-get event :type))
         (buffer (or (pi-ui--get-buffer session)
                     (when (equal event-type "extension_ui_request")
                       (pi-ui--ensure-buffer session)))))
    (when (buffer-live-p buffer)
      (pcase event-type
        ("message_start"
         (when (equal (pi-ui--event-message-role event) "assistant")
           (with-current-buffer buffer
             (setq-local pi-ui--live-message (plist-get event :message)))
           (pi-ui--schedule-render buffer t)))
        ("message_update"
         (when (pi-ui--message-update-assistant-p event)
           (with-current-buffer buffer
             (setq-local pi-ui--live-message (plist-get event :message)))
           (if pi-ui-enable-streaming
               (pi-ui--schedule-render buffer)
             (pi-ui--schedule-render buffer t))))
        ("message_end"
         (let ((message (plist-get event :message)))
           (pcase (plist-get message :role)
             ("assistant"
              (with-current-buffer buffer
                (setq-local pi-ui--live-message message))
              (pi-ui--schedule-render buffer t))
             ("user"
              (pi-ui--append-history-message buffer message))
             ("toolResult" nil))))
        ((or "tool_execution_start" "tool_execution_update" "tool_execution_end")
         (pi-ui--upsert-tool-transient buffer (pi-ui--tool-result-from-event event)))
        ("queue_update"
         (pi-ui--update-session-header-line buffer))
        ((or "compaction_start" "compaction_end"
             "auto_retry_start" "auto_retry_end")
         (pi-ui--append-transient buffer (pi-ui--status-item-from-event event))
         (pi-ui--update-session-header-line buffer))
        ("extension_ui_request"
         (pi-ui--handle-extension-ui-event session buffer event))
        ("extension_error"
         (pi-ui--append-transient
          buffer
          (list :kind 'error
                :text (or (plist-get event :error)
                          "Extension error"))))
        ((or "session_error" "session_exit")
         (let ((text (or (plist-get event :error)
                         (plist-get event :event)
                         "Unknown session error")))
           (message "pi: %s" text)
           (pi-ui--append-transient buffer (list :kind 'error :text text))))
        ("agent_end"
         (pi-ui--refresh-session-buffer session buffer))
        (_ nil)))))

(defun pi-ui-toggle-tool-display-style ()
  "Cycle `pi-ui-tool-display-style' and re-render current session buffer."
  (interactive)
  (setq pi-ui-tool-display-style
        (pcase pi-ui-tool-display-style
          ('compact 'verbose)
          ('verbose 'hidden)
          (_ 'compact)))
  (message "pi tool display: %s" pi-ui-tool-display-style)
  (when (derived-mode-p 'pi-session-buffer-mode)
    (pi-ui--buffer-render (current-buffer))))

(defun pi-ui-toggle-auto-scroll ()
  "Toggle `pi-ui-auto-scroll' for session output."
  (interactive)
  (setq pi-ui-auto-scroll (not pi-ui-auto-scroll))
  (message "pi auto-scroll: %s" (if pi-ui-auto-scroll "on" "off"))
  (when (derived-mode-p 'pi-session-buffer-mode)
    (pi-ui--buffer-render (current-buffer))))

(defun pi-ui--session-buffer-refresh ()
  "Reload the current session buffer from session history."
  (unless (and (boundp 'pi-ui--session) pi-ui--session)
    (user-error "Not in a pi session buffer"))
  (pi-ui--refresh-session-buffer pi-ui--session (current-buffer)))

(defun pi-ui--session-buffer-clear-and-reload ()
  "Clear the current session buffer state and reload from session history."
  (unless (and (boundp 'pi-ui--session) pi-ui--session)
    (user-error "Not in a pi session buffer"))
  (setq-local pi-ui--history nil
              pi-ui--live-message nil
              pi-ui--transient-items nil)
  (pi-ui--refresh-session-buffer pi-ui--session (current-buffer)))

(defun pi-ui--session-buffer-jump-to-source ()
  "Jump back to the last remembered source buffer for this session buffer."
  (unless (buffer-live-p pi-ui--source-buffer)
    (user-error "No source buffer recorded for this session"))
  (pop-to-buffer pi-ui--source-buffer))

(defun pi-ui--session-buffer-abort ()
  "Abort the current request for this session."
  (unless (and (boundp 'pi-ui--session) pi-ui--session)
    (user-error "Not in a pi session buffer"))
  (pi-session-abort pi-ui--session
                    (lambda (_session response)
                      (when (not (pi-rpc-success-p response))
                        (message "pi abort failed: %s" (plist-get response :error))))))

(defun pi-ui--session-buffer-compose-prompt ()
  "Open the prompt composer for the current session's source buffer."
  (interactive)
  (unless (and (boundp 'pi-ui--session) pi-ui--session)
    (user-error "Not in a pi session buffer"))
  (pi-ui-compose-prompt (or pi-ui--source-buffer (current-buffer))))

(provide 'pi-session-buffer)

;;; pi-session-buffer.el ends here
