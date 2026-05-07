;;; pi-render.el --- Rendering helpers for pi session buffers -*- lexical-binding: t; -*-

;; Author: Fangyuan and contributors
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Rendering helpers for pi session and prompt buffers.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'pi-rpc)
(require 'pi-session)

(declare-function pi-rpc-json-truthy-p "pi-rpc" (value))
(declare-function pi-rpc-success-p "pi-rpc" (response))

(defvar pi-ui-show-thinking)
(defvar pi-ui-tool-result-max-lines)
(defvar pi-ui-tool-display-style)

(defconst pi-ui--spinner-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])

(defvar pi-ui--spinner-timer nil)
(defvar pi-ui--spinner-frame-index 0)

(defvar pi-ui-session-title-face)
(defvar pi-ui-meta-face)
(defvar pi-ui-user-heading-face)
(defvar pi-ui-assistant-heading-face)
(defvar pi-ui-tool-heading-face)
(defvar pi-ui-tool-line-face)
(defvar pi-ui-tool-prefix-face)
(defvar pi-ui-tool-success-face)
(defvar pi-ui-tool-error-face)
(defvar pi-ui-placeholder-face)

(defvar-local pi-ui--session nil)
(defvar-local pi-ui--tool-call-summary-by-id nil)

(defun pi-ui--session-model-name (session)
  (when-let* ((state (and session (pi-session-cached-state session)))
              (model (plist-get state :model)))
    (or (plist-get model :name)
        (let ((provider (plist-get model :provider))
              (id (plist-get model :id)))
          (cond
           ((and provider id) (format "%s/%s" provider id))
           (id id))))))

(defun pi-ui--spinner-frame ()
  (aref pi-ui--spinner-frames
        (mod pi-ui--spinner-frame-index (length pi-ui--spinner-frames))))

(defun pi-ui--active-session-buffer-p (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (and (bound-and-true-p pi-ui--session)
           (let ((state (pi-session-cached-state pi-ui--session)))
             (or (pi-rpc-json-truthy-p (plist-get state :is-streaming))
                 (pi-rpc-json-truthy-p (plist-get state :is-compacting))
                 (pi-rpc-json-truthy-p (plist-get state :is-retrying))))
           (get-buffer-window buffer t)))))

(defun pi-ui--stop-spinner-timer ()
  (when (timerp pi-ui--spinner-timer)
    (cancel-timer pi-ui--spinner-timer))
  (setq pi-ui--spinner-timer nil
        pi-ui--spinner-frame-index 0))

(defun pi-ui--spinner-tick ()
  (let ((buffers (seq-filter #'pi-ui--active-session-buffer-p
                             (buffer-list))))
    (if (null buffers)
        (pi-ui--stop-spinner-timer)
      (setq pi-ui--spinner-frame-index (1+ pi-ui--spinner-frame-index))
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (force-mode-line-update))))))

(defun pi-ui--ensure-spinner-timer ()
  (if (seq-some #'pi-ui--active-session-buffer-p (buffer-list))
      (unless (timerp pi-ui--spinner-timer)
        (setq pi-ui--spinner-timer (run-at-time 0 0.1 #'pi-ui--spinner-tick)))
    (pi-ui--stop-spinner-timer)))

(defun pi-ui--session-header-line (session)
  (when session
    (let* ((state (pi-session-cached-state session))
           (items nil))
      (push (cond
             ((pi-rpc-json-truthy-p (plist-get state :is-compacting))
              (format "%s Compacting" (pi-ui--spinner-frame)))
             ((pi-rpc-json-truthy-p (plist-get state :is-retrying))
              (format "%s Retrying" (pi-ui--spinner-frame)))
             ((pi-rpc-json-truthy-p (plist-get state :is-streaming))
              (format "%s Working" (pi-ui--spinner-frame)))
             (t "○ Idle"))
            items)
      (when-let* ((model-name (pi-ui--session-model-name session)))
        (push (format "Model: %s" model-name) items))
      (when-let* ((thinking-level (plist-get state :thinking-level)))
        (push (format "Thinking: %s" thinking-level) items))
      (when-let* ((pending (plist-get state :pending-message-count))
                  ((numberp pending))
                  ((> pending 0)))
        (push (format "Queued: %d" pending) items))
      (when (pi-rpc-json-truthy-p (plist-get state :auto-compaction-enabled))
        (push "Auto-compact" items))
      (when (pi-rpc-json-truthy-p (plist-get state :auto-retry-enabled))
        (push "Auto-retry" items))
      (propertize (string-join (nreverse items) " • ")
                  'face 'pi-ui-meta-face))))

(defun pi-ui--update-session-header-line (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (setq-local header-line-format
                '(:eval
                  (and (boundp 'pi-ui--session)
                       (pi-ui--session-header-line pi-ui--session))))
    (pi-ui--ensure-spinner-timer)
    (force-mode-line-update)))

(defun pi-ui--stringify-content-block (block)
  (pcase (plist-get block :type)
    ("text" (or (plist-get block :text) ""))
    ("image" "[image]")
    (_ "")))

(defun pi-ui--extract-user-content (message)
  (let ((content (plist-get message :content)))
    (cond
     ((stringp content) content)
     ((listp content)
      (string-join
       (delq nil
             (mapcar (lambda (block)
                       (let ((text (pi-ui--stringify-content-block block)))
                         (unless (string-empty-p text) text)))
                     content))
       "\n"))
     (t ""))))

(defun pi-ui--extract-assistant-content (message)
  (let ((content (plist-get message :content))
        (pieces nil))
    (dolist (block content)
      (pcase (plist-get block :type)
        ("thinking"
         (when pi-ui-show-thinking
           (let ((text (or (plist-get block :text)
                           (plist-get block :thinking)
                           "[thinking]")))
             (unless (string-empty-p text)
               (push (propertize text 'face 'pi-ui-placeholder-face) pieces)))))
        ("text"
         (let ((text (plist-get block :text)))
           (unless (string-empty-p (or text ""))
             (push text pieces))))))
    (string-join (nreverse pieces) "\n\n")))

(defun pi-ui--display-text (text)
  "Return TEXT with terminal escape sequences removed for buffer display."
  (cond
   ((null text) "")
   ((stringp text) (ansi-color-filter-apply text))
   (t (ansi-color-filter-apply (format "%s" text)))))

(defun pi-ui--truncate-lines (text max-lines)
  (let* ((lines (split-string (or text "") "\n"))
         (count (length lines)))
    (if (<= count max-lines)
        (string-join lines "\n")
      (concat (string-join (seq-take lines max-lines) "\n")
              (format "\n... [%d more lines hidden]" (- count max-lines))))))

(defun pi-ui--truncate-inline (text max-length)
  (let* ((raw (cond
               ((null text) "")
               ((stringp text) text)
               (t (format "%s" text))))
         (flat (replace-regexp-in-string "[\n\r\t]+" " " raw))
         (trimmed (string-trim flat)))
    (if (> (length trimmed) max-length)
        (concat (substring trimmed 0 (max 0 (1- max-length))) "…")
      trimmed)))

(defun pi-ui--tool-arg-value (args key)
  (let* ((name (if (symbolp key) (symbol-name key) key))
         (plain (if (string-prefix-p ":" name) (substring name 1) name))
         (kw (intern (concat ":" plain)))
         (sym (intern plain)))
    (cond
     ((hash-table-p args)
      (or (gethash kw args)
          (gethash sym args)
          (gethash plain args)))
     ((listp args)
      (or (plist-get args kw)
          (plist-get args sym)
          (cdr (assq kw args))
          (cdr (assq sym args))
          (cdr (assoc plain args))))
     (t nil))))

(defun pi-ui--normalize-tool-arguments (value)
  (cond
   ((or (null value) (listp value) (hash-table-p value)) value)
   ((stringp value)
    (condition-case nil
        (let ((json-object-type 'plist)
              (json-array-type 'list)
              (json-false :json-false)
              (json-null nil))
          (json-parse-string value :object-type 'plist :array-type 'list
                             :false-object :json-false :null-object nil))
      (error nil)))
   (t nil)))

(defun pi-ui--tool-arguments-from-object (object)
  (pi-ui--normalize-tool-arguments
   (or (plist-get object :arguments)
       (plist-get object :input)
       (plist-get object :params)
       (plist-get object :parameters)
       (plist-get object :toolArguments)
       (plist-get object :toolInput))))

(defun pi-ui--display-tool-path (path)
  (when (and (stringp path) (not (string-empty-p path)))
    (let* ((root (and pi-ui--session
                      (pi-session-root pi-ui--session)
                      (file-name-as-directory
                       (expand-file-name (pi-session-root pi-ui--session)))))
           (rendered
            (if root
                (let ((expanded (expand-file-name path root)))
                  (if (string-prefix-p root expanded)
                      (file-relative-name expanded root)
                    (abbreviate-file-name expanded)))
              (if (file-name-absolute-p path)
                  (abbreviate-file-name path)
                path))))
      (pi-ui--truncate-inline rendered 90))))

(defun pi-ui--tool-detail-from-arguments (tool-name args)
  (let* ((tool (and (stringp tool-name) (downcase tool-name)))
         (args (pi-ui--normalize-tool-arguments args))
         (detail
          (pcase tool
            ((or "read" "write" "edit")
             (pi-ui--display-tool-path (pi-ui--tool-arg-value args "path")))
            ("bash"
             (when-let* ((command (pi-ui--tool-arg-value args "command")))
               (concat "$ " (pi-ui--truncate-inline command 70))))
            ("arxiv_paper"
             (pi-ui--truncate-inline (or (pi-ui--tool-arg-value args "id") "") 60))
            ("arxiv_search"
             (pi-ui--truncate-inline (or (pi-ui--tool-arg-value args "query") "") 60))
            ("zotero"
             (let ((action (pi-ui--tool-arg-value args "action"))
                   (query (pi-ui--tool-arg-value args "query")))
               (cond
                ((and action query)
                 (pi-ui--truncate-inline (format "%s: %s" action query) 60))
                (action (format "%s" action))
                (query (pi-ui--truncate-inline query 60))
                (t nil))))
            ("zotero_web"
             (let ((action (pi-ui--tool-arg-value args "action"))
                   (query (pi-ui--tool-arg-value args "query")))
               (cond
                ((and action query)
                 (pi-ui--truncate-inline (format "%s: %s" action query) 60))
                (action (format "%s" action))
                (query (pi-ui--truncate-inline query 60))
                (t nil))))
            (_
             (or (pi-ui--display-tool-path (pi-ui--tool-arg-value args "path"))
                 (when-let* ((query (pi-ui--tool-arg-value args "query")))
                   (pi-ui--truncate-inline query 60))
                 (when-let* ((id (pi-ui--tool-arg-value args "id")))
                   (pi-ui--truncate-inline (format "%s" id) 60))
                 (when-let* ((command (pi-ui--tool-arg-value args "command")))
                   (concat "$ " (pi-ui--truncate-inline command 60))))))))
    (and (stringp detail) (not (string-empty-p detail)) detail)))

(defun pi-ui--rebuild-tool-call-summary-index (history live)
  (let ((table (make-hash-table :test #'equal)))
    (cl-labels
        ((collect (message)
           (when (and (listp message)
                      (equal (plist-get message :role) "assistant"))
             (dolist (block (plist-get message :content))
               (when (and (listp block)
                          (equal (plist-get block :type) "toolCall"))
                 (let* ((tool-call-ids
                         (delq nil
                               (list (plist-get block :id)
                                     (plist-get block :toolCallId)
                                     (plist-get block :toolUseId)
                                     (plist-get block :callId))))
                        (tool-name (or (plist-get block :name)
                                       (plist-get block :toolName)))
                        (args (pi-ui--tool-arguments-from-object block))
                        (detail (pi-ui--tool-detail-from-arguments tool-name args)))
                   (when detail
                     (dolist (tool-call-id tool-call-ids)
                       (when (and (stringp tool-call-id)
                                  (not (string-empty-p tool-call-id)))
                         (puthash tool-call-id detail table))))))))))
      (dolist (message history)
        (collect message))
      (collect live))
    (setq-local pi-ui--tool-call-summary-by-id table)))

(defun pi-ui--tool-detail-for-call-id (tool-call-id)
  (when (and (stringp tool-call-id)
             (hash-table-p pi-ui--tool-call-summary-by-id)
             (not (string-empty-p tool-call-id)))
    (gethash tool-call-id pi-ui--tool-call-summary-by-id)))

(defun pi-ui--tool-detail-from-message (message)
  (or (pi-ui--tool-detail-from-arguments
       (plist-get message :toolName)
       (pi-ui--tool-arguments-from-object message))
      (pi-ui--tool-detail-for-call-id (plist-get message :toolCallId))
      (pi-ui--tool-detail-for-call-id (plist-get message :toolUseId))
      (pi-ui--tool-detail-for-call-id (plist-get message :callId))))

(defun pi-ui--tool-detail-from-item (item)
  (or (plist-get item :detail)
      (pi-ui--tool-detail-from-arguments
       (plist-get item :tool-name)
       (plist-get item :arguments))
      (pi-ui--tool-detail-for-call-id (plist-get item :tool-call-id))))

(defun pi-ui--extract-tool-result-content (message)
  (let ((content (plist-get message :content)))
    (if (listp content)
        (string-join
         (delq nil
               (mapcar (lambda (block)
                         (let ((text (pi-ui--stringify-content-block block)))
                           (unless (string-empty-p text) text)))
                       content))
         "\n")
      "")))

(defun pi-ui--tool-status-from-event (event)
  (let* ((result (plist-get event :result))
         (result-error (and result
                            (or (plist-get result :error)
                                (eq (plist-get result :isError) t)
                                (and (plist-member result :success)
                                     (not (pi-rpc-success-p result)))))))
    (cond
     ((or (plist-get event :error)
          (eq (plist-get event :isError) t)
          result-error)
      'error)
     (result 'success)
     (t 'unknown))))

(defun pi-ui--tool-status-from-message (message)
  (cond
   ((or (plist-get message :error)
        (eq (plist-get message :isError) t)
        (and (plist-member message :success)
             (not (pi-rpc-success-p message))))
    'error)
   ((plist-get message :content) 'success)
   (t 'unknown)))

(defun pi-ui--tool-duration-from-event (event)
  (let ((result (plist-get event :result)))
    (or (plist-get event :durationMs)
        (plist-get event :elapsedMs)
        (and result (plist-get result :durationMs))
        (and result (plist-get result :elapsedMs)))))

(defun pi-ui--format-duration-ms (value)
  (when (numberp value)
    (if (< value 1000)
        (format "%d ms" (truncate value))
      (format "%.2f s" (/ value 1000.0)))))

(defun pi-ui--tool-summary-face (status)
  (pcase status
    ('success 'pi-ui-tool-success-face)
    ('error 'pi-ui-tool-error-face)
    (_ 'pi-ui-tool-line-face)))

(defun pi-ui--section-label (text face &optional level)
  (let ((level (max 1 (or level 3))))
    (propertize (format "%s %s\n\n" (make-string level ?#) text)
                'face face)))

(defun pi-ui--render-tool-result (tool-name &optional text status duration-ms detail)
  (pcase pi-ui-tool-display-style
    ('hidden "")
    ('verbose
     (let* ((label (string-join (delq nil (list (or tool-name "tool") detail)) " · "))
            (suffix (string-join
                     (delq nil
                           (list
                            (pcase status
                              ('success "✓")
                              ('error "✗")
                              ('running "…")
                              (_ nil))
                            (when-let* ((duration (pi-ui--format-duration-ms duration-ms)))
                              (format "(%s)" duration))))
                     " ")))
       (concat (pi-ui--section-label
                (string-trim
                 (format "Tool Result · %s %s"
                         label
                         suffix))
                'pi-ui-tool-heading-face)
               "```text\n"
               (pi-ui--truncate-lines (or text "") pi-ui-tool-result-max-lines)
               "\n```\n\n")))
    (_
     (let* ((tool (or tool-name "tool"))
            (detail-text (and (stringp detail)
                              (not (string-empty-p detail))
                              (propertize (concat " · " detail) 'face 'pi-ui-tool-line-face)))
            (status-mark (pcase status
                           ('success (propertize "✓" 'face 'pi-ui-tool-success-face))
                           ('error (propertize "✗" 'face 'pi-ui-tool-error-face))
                           ('running (propertize "…" 'face 'pi-ui-tool-line-face))
                           (_ nil)))
            (duration (when-let* ((formatted (pi-ui--format-duration-ms duration-ms)))
                        (propertize (format "(%s)" formatted) 'face 'pi-ui-tool-line-face))))
       (concat (propertize "Tool · " 'face 'pi-ui-tool-prefix-face)
               (propertize tool 'face (pi-ui--tool-summary-face status))
               (or detail-text "")
               (if status-mark (concat " " status-mark) "")
               (if duration (concat " " duration) "")
               "\n")))))

(defun pi-ui--render-message (message)
  (pcase (plist-get message :role)
    ("user"
     (concat (pi-ui--section-label "User" 'pi-ui-user-heading-face 2)
             (pi-ui--extract-user-content message)
             "\n\n"))
    ("assistant"
     (let ((content (pi-ui--extract-assistant-content message)))
       (if (string-empty-p content)
           ""
         (concat (pi-ui--section-label "Assistant" 'pi-ui-assistant-heading-face 2)
                 content
                 "\n\n"))))
    ("toolResult"
     (pi-ui--render-tool-result
      (plist-get message :toolName)
      (pi-ui--extract-tool-result-content message)
      (pi-ui--tool-status-from-message message)
      nil
      (pi-ui--tool-detail-from-message message)))
    ("custom"
     (concat (pi-ui--section-label "Custom" 'pi-ui-meta-face)
             (or (pi-ui--extract-user-content message) "")
             "\n\n"))
    (_ "")))

(defun pi-ui--render-extension-state (statuses widgets title)
  (let (lines)
    (when (and (stringp title) (not (string-empty-p title)))
      (push (format "Title: %s" (pi-ui--display-text title)) lines))
    (dolist (entry (reverse statuses))
      (let ((key (car entry))
            (text (cdr entry)))
        (when (and (stringp text) (not (string-empty-p text)))
          (push (format "%s: %s" key (pi-ui--display-text text)) lines))))
    (dolist (widget (reverse widgets))
      (pcase-let ((`(,key ,widget-lines ,_placement) widget))
        (push (format "Widget %s:" key) lines)
        (dolist (line widget-lines)
          (push (format "  %s" (pi-ui--display-text line)) lines))))
    (if lines
        (concat (propertize "Extension UI\n" 'face 'pi-ui-meta-face)
                (propertize (string-join (nreverse lines) "\n")
                            'face 'pi-ui-meta-face)
                "\n\n")
      "")))

(defun pi-ui--render-transient-item (item)
  (pcase (plist-get item :kind)
    ('notify
     (concat (propertize "> " 'face 'pi-ui-meta-face)
             (plist-get item :text)
             "\n\n"))
    ('error
     (concat (propertize "> Error: " 'face 'error)
             (plist-get item :text)
             "\n\n"))
    ('extension
     (concat (propertize "> " 'face 'pi-ui-meta-face)
             (plist-get item :text)
             "\n\n"))
    ('status
     (concat (propertize "> " 'face 'pi-ui-meta-face)
             (plist-get item :text)
             "\n\n"))
    ('tool-result
     (pi-ui--render-tool-result
      (plist-get item :tool-name)
      (or (plist-get item :text) "")
      (plist-get item :status)
      (plist-get item :duration-ms)
      (pi-ui--tool-detail-from-item item)))
    (_ "")))

(defun pi-ui--normalize-message-content (content)
  (cond
   ((listp content) content)
   ((stringp content)
    (if (string-empty-p content)
        nil
      (list (list :type "text" :text content))))
   (t nil)))

(provide 'pi-render)

;;; pi-render.el ends here
