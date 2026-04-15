;;; pi-session-buffer.el --- Session buffer UI for pi -*- lexical-binding: t; -*-

;; Author: Fangyuan and contributors
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Session buffer management, rendering, and event handling for pi.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pi-session)
(require 'pi-prompt)
(require 'pi-render)
(require 'pi-extension-ui)
(require 'markdown-mode nil t)

(declare-function pi-session-buffer-mode "pi-session-buffer" ())

(defvar pi-ui-window-side)
(defvar pi-ui-window-size)
(defvar pi-ui-auto-scroll)
(defvar pi-ui-enable-streaming)
(defvar pi-ui-stream-render-interval)
(defvar pi-ui-tool-display-style)

(defvar pi-ui-session-title-face)
(defvar pi-ui-meta-face)
(defvar pi-ui-placeholder-face)

(defvar pi-ui--session-buffers (make-hash-table :test #'equal))

(defvar-local pi-ui--session nil)
(defvar-local pi-ui--source-buffer nil)
(defvar-local pi-ui--history nil)
(defvar-local pi-ui--live-message nil)
(defvar-local pi-ui--transient-items nil)
(defvar-local pi-ui--loading nil)
(defvar-local pi-ui--pending-render-timer nil)
(defvar-local pi-ui--tool-call-summary-by-id nil)

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
       (if (eq (plist-get response :success) :json-false)
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
       (when (eq (plist-get response :success) :json-false)
         (message "pi: %s" (plist-get response :error)))))
    (when (window-live-p source-window)
      (select-window source-window))
    buffer))

(defun pi-ui--event-message-role (event)
  (let ((message (plist-get event :message)))
    (and message (plist-get message :role))))

(defun pi-ui--assistant-event-p (event)
  (or (equal (pi-ui--event-message-role event) "assistant")
      (equal (plist-get event :role) "assistant")
      (equal (plist-get event :messageRole) "assistant")))

(defun pi-ui--event-text-delta (event)
  (let ((delta (or (plist-get event :delta)
                   (plist-get event :text)
                   (plist-get event :contentDelta)
                   (plist-get event :textDelta))))
    (cond
     ((stringp delta) delta)
     ((listp delta)
      (or (plist-get delta :text)
          (let ((content (plist-get delta :content)))
            (when (listp content)
              (string-join
               (delq nil
                     (mapcar (lambda (block)
                               (let ((text (pi-ui--stringify-content-block block)))
                                 (unless (string-empty-p text) text)))
                             content))
               "\n")))))
     (t nil))))

(defun pi-ui--append-live-delta (buffer text)
  (when (and (stringp text) (not (string-empty-p text)))
    (with-current-buffer buffer
      (let* ((message (or pi-ui--live-message (list :role "assistant" :content nil)))
             (content (pi-ui--normalize-message-content (plist-get message :content))))
        (if (and content
                 (equal (plist-get (car (last content)) :type) "text"))
            (let* ((prefix (butlast content))
                   (last-block (car (last content)))
                   (updated-last (list :type "text"
                                       :text (concat (or (plist-get last-block :text) "") text))))
              (setq content (append prefix (list updated-last))))
          (setq content (append content (list (list :type "text" :text text)))))
        (setq-local pi-ui--live-message
                    (plist-put message :content content))))))

(defun pi-ui--tool-result-from-event (event)
  (let* ((result (plist-get event :result))
         (tool-name (or (plist-get event :toolName)
                        (and result (plist-get result :toolName))))
         (tool-call-id (or (plist-get event :toolCallId)
                           (plist-get event :toolUseId)
                           (plist-get event :callId)
                           (and result (plist-get result :toolCallId))
                           (and result (plist-get result :toolUseId))
                           (and result (plist-get result :callId))))
         (arguments (or (pi-ui--tool-arguments-from-object event)
                        (and result (pi-ui--tool-arguments-from-object result))))
         (content (and result (plist-get result :content))))
    (when (or result (plist-get event :error))
      (list :kind 'tool-result
            :tool-name tool-name
            :tool-call-id tool-call-id
            :arguments arguments
            :detail (pi-ui--tool-detail-from-arguments tool-name arguments)
            :status (pi-ui--tool-status-from-event event)
            :duration-ms (pi-ui--tool-duration-from-event event)
            :text (cond
                   ((listp content)
                    (string-join
                     (delq nil
                           (mapcar (lambda (block)
                                     (let ((text (pi-ui--stringify-content-block block)))
                                       (unless (string-empty-p text) text)))
                                   content))
                     "\n"))
                   ((stringp content) content)
                   (t ""))))))

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
         (let ((message (plist-get event :message)))
           (when (and message (equal (plist-get message :role) "assistant"))
             (with-current-buffer buffer
               (setq-local pi-ui--live-message message))
             (if pi-ui-enable-streaming
                 (pi-ui--schedule-render buffer)
               (pi-ui--schedule-render buffer t)))))
        ((or "message_delta" "content_delta" "assistant_delta" "text_delta")
         (when (and pi-ui-enable-streaming (pi-ui--assistant-event-p event))
           (when-let* ((delta (pi-ui--event-text-delta event)))
             (pi-ui--append-live-delta buffer delta)
             (pi-ui--schedule-render buffer))))
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
        ("tool_execution_end"
         (when-let* ((item (pi-ui--tool-result-from-event event)))
           (pi-ui--append-transient buffer item)))
        ("extension_ui_request"
         (pi-ui--handle-extension-ui-event session buffer event))
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
                      (when (eq (plist-get response :success) :json-false)
                        (message "pi abort failed: %s" (plist-get response :error))))))

(defun pi-ui--session-buffer-compose-prompt ()
  "Open the prompt composer for the current session's source buffer."
  (interactive)
  (unless (and (boundp 'pi-ui--session) pi-ui--session)
    (user-error "Not in a pi session buffer"))
  (pi-ui-compose-prompt (or pi-ui--source-buffer (current-buffer))))

(provide 'pi-session-buffer)

;;; pi-session-buffer.el ends here
