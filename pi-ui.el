;;; pi-ui.el --- Session response buffer UI for pi -*- lexical-binding: t; -*-

;; Author: Fangyuan and contributors
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Render pi session output into one Emacs buffer per session.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'pi-session)
(require 'markdown-mode nil t)

(declare-function pi-session-get-commands "pi-session" (session callback))

(defgroup pi-ui nil
  "UI for pi session buffers."
  :group 'applications)

(defcustom pi-ui-window-side 'right
  "Default side for session buffers."
  :type '(choice (const right) (const bottom) (const left) (const top)))

(defcustom pi-ui-window-size 0.33
  "Default side-window size for session buffers.
Fraction for right/left windows, or number of lines for top/bottom windows."
  :type 'number)

(defcustom pi-ui-tool-result-max-lines 5
  "Maximum number of tool-result lines to show in verbose tool display mode."
  :type 'integer)

(defcustom pi-ui-tool-display-style 'compact
  "How to render tool usage in session buffers.

`compact' shows only tool name/status in one line.
`verbose' shows tool output blocks.
`hidden' suppresses tool usage lines."
  :type '(choice (const compact) (const verbose) (const hidden)))

(defcustom pi-ui-show-thinking nil
  "Whether to show assistant thinking blocks.

When nil, thinking blocks are omitted from rendering."
  :type 'boolean)

(defcustom pi-ui-auto-scroll t
  "Whether pi session windows should follow new output automatically.

When non-nil, windows that are already at the bottom will stay pinned
to the latest output during updates."
  :type 'boolean)

(defcustom pi-ui-enable-streaming t
  "Whether to render incremental assistant deltas while a run is active."
  :type 'boolean)

(defcustom pi-ui-stream-render-interval 0.05
  "Seconds between live-stream redraws.

Lower values feel more immediate but may increase UI load."
  :type 'number)

(defcustom pi-ui-prompt-window-side 'bottom
  "Default side for prompt composer buffers."
  :type '(choice (const right) (const bottom) (const left) (const top)))

(defcustom pi-ui-prompt-window-size 12
  "Default side-window size for prompt composer buffers."
  :type 'number)

(defcustom pi-ui-include-active-region t
  "Whether `pi-ui-compose-prompt' should include active region text as context."
  :type 'boolean)

(defcustom pi-ui-region-context-max-chars 8000
  "Maximum number of characters copied from an active region into prompt context."
  :type 'integer)

(defcustom pi-ui-fallback-file-scan-max-files 1000
  "Maximum number of files to collect for @file completion outside projects."
  :type 'integer)

(defcustom pi-ui-fallback-file-scan-max-depth 3
  "Maximum directory depth for @file completion outside projects."
  :type 'integer)

(defface pi-ui-session-title-face
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1))
  "Face for session buffer titles."
  :group 'pi-ui)

(defface pi-ui-meta-face
  '((t :inherit shadow))
  "Face for session metadata."
  :group 'pi-ui)

(defface pi-ui-user-heading-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for user section headings."
  :group 'pi-ui)

(defface pi-ui-assistant-heading-face
  '((t :inherit success :weight bold))
  "Face for assistant section headings."
  :group 'pi-ui)

(defface pi-ui-tool-heading-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for tool-result section headings."
  :group 'pi-ui)

(defface pi-ui-tool-line-face
  '((t :inherit shadow))
  "Face for compact tool lines."
  :group 'pi-ui)

(defface pi-ui-tool-prefix-face
  '((t :inherit pi-ui-meta-face))
  "Face for compact tool line prefix text."
  :group 'pi-ui)

(defface pi-ui-tool-success-face
  '((t :inherit success))
  "Face for successful compact tool lines."
  :group 'pi-ui)

(defface pi-ui-tool-error-face
  '((t :inherit error))
  "Face for failed compact tool lines."
  :group 'pi-ui)

(defface pi-ui-placeholder-face
  '((t :inherit shadow :slant italic))
  "Face for hidden/thin placeholder text."
  :group 'pi-ui)

(defvar pi-ui--session-buffers (make-hash-table :test #'equal))

(defvar-local pi-ui--session nil)
(defvar-local pi-ui--source-buffer nil)
(defvar-local pi-ui--history nil)
(defvar-local pi-ui--live-message nil)
(defvar-local pi-ui--transient-items nil)
(defvar-local pi-ui--loading nil)
(defvar-local pi-ui--pending-render-timer nil)
(defvar-local pi-ui--tool-call-summary-by-id nil)

(defvar-local pi-ui--prompt-source-buffer nil)
(defvar-local pi-ui--prompt-source-window nil)
(defvar-local pi-ui--prompt-file-cache nil)
(defvar-local pi-ui--prompt-command-cache nil)
(defvar-local pi-ui--prompt-command-metadata nil)
(defvar-local pi-ui--prompt-command-loading nil)

(defvar pi-prompt-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pi-ui-prompt-submit)
    (define-key map (kbd "C-c C-k") #'pi-ui-prompt-cancel)
    (define-key map (kbd "C-c C-f") #'pi-ui-prompt-insert-file-reference)
    (define-key map (kbd "C-c C-s") #'pi-ui-prompt-insert-command)
    (define-key map (kbd "TAB") #'completion-at-point)
    map)
  "Keymap for `pi-prompt-buffer-mode'.")

(define-derived-mode pi-prompt-buffer-mode text-mode "Pi-Prompt"
  "Major mode for composing longer pi prompts."
  (setq-local header-line-format
              "Pi prompt: C-c C-c send • C-c C-k cancel • C-c C-f @file • C-c C-s /command")
  (setq-local pi-ui--prompt-source-buffer nil)
  (setq-local pi-ui--prompt-source-window nil)
  (setq-local pi-ui--prompt-file-cache nil)
  (setq-local pi-ui--prompt-command-cache nil)
  (setq-local pi-ui--prompt-command-metadata nil)
  (setq-local pi-ui--prompt-command-loading nil)
  (add-hook 'completion-at-point-functions #'pi-ui--prompt-completion-at-point nil t))

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

(defun pi-ui--session-model-name (session)
  (when-let* ((state (and session (pi-session-cached-state session)))
              (model (plist-get state :model)))
    (or (plist-get model :name)
        (let ((provider (plist-get model :provider))
              (id (plist-get model :id)))
          (cond
           ((and provider id) (format "%s/%s" provider id))
           (id id))))))

(defun pi-ui--session-header-line (session)
  (when session
    (let ((items nil))
      (when-let* ((model-name (pi-ui--session-model-name session)))
        (push (format "Model: %s" model-name) items))
      (when-let* ((thinking-level (plist-get (pi-session-cached-state session)
                                             :thinking-level)))
        (push (format "Thinking: %s" thinking-level) items))
      (when items
        (propertize (string-join (nreverse items) " • ")
                    'face 'pi-ui-meta-face)))))

(defun pi-ui--update-session-header-line (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (setq-local header-line-format
                '(:eval
                  (and (boundp 'pi-ui--session)
                       (pi-ui--session-header-line pi-ui--session))))
    (force-mode-line-update t)))

(defun pi-ui--prompt-source-root (source-buffer)
  (or (and (buffer-live-p source-buffer)
           (plist-get (pi-session-scope-for-buffer source-buffer) :root))
      default-directory))

(defun pi-ui--prompt-source-name (source-buffer)
  (if-let* ((file (and (buffer-live-p source-buffer)
                       (buffer-file-name source-buffer))))
      file
    (if (buffer-live-p source-buffer)
        (buffer-name source-buffer)
      "unknown")))

(defun pi-ui--prompt-buffer-name (source-buffer)
  (format "*pi-prompt:%s*"
          (if (buffer-live-p source-buffer)
              (buffer-name source-buffer)
            "detached")))

(defun pi-ui--capture-region-context (source-buffer)
  (when (and pi-ui-include-active-region (buffer-live-p source-buffer))
    (with-current-buffer source-buffer
      (when (use-region-p)
        (let* ((beg (region-beginning))
               (end (region-end))
               (line-beg (line-number-at-pos beg t))
               (line-end (line-number-at-pos end t))
               (raw (buffer-substring-no-properties beg end))
               (truncated (> (length raw) pi-ui-region-context-max-chars))
               (text (if truncated
                         (substring raw 0 pi-ui-region-context-max-chars)
                       raw))
               (source-name (pi-ui--prompt-source-name source-buffer)))
          (concat
           (format "Selected context (%s:%d-%d):\n" source-name line-beg line-end)
           "```text\n"
           text
           (if truncated
               (format "\n... [truncated to %d chars]" pi-ui-region-context-max-chars)
             "")
           "\n```"))))))

(defun pi-ui--display-prompt-buffer (buffer)
  (display-buffer-in-side-window
   buffer
   `((side . ,pi-ui-prompt-window-side)
     ,@(if (memq pi-ui-prompt-window-side '(left right))
           `((window-width . ,pi-ui-prompt-window-size))
         `((window-height . ,pi-ui-prompt-window-size))))))

(defun pi-ui--prompt-fallback-ignore-directory-p (name)
  (or (string-prefix-p "." name)
      (member name '("node_modules" "venv" ".venv" "__pycache__" ".direnv"))))

(defun pi-ui--prompt-fallback-file-candidates (root)
  (let ((root (file-name-as-directory (expand-file-name root)))
        (queue (list (cons (file-name-as-directory (expand-file-name root)) 0)))
        (results nil)
        (count 0))
    (while (and queue (< count pi-ui-fallback-file-scan-max-files))
      (pcase-let ((`(,dir . ,depth) (pop queue)))
        (dolist (name (directory-files dir nil nil t))
          (unless (member name '("." ".."))
            (let ((path (expand-file-name name dir)))
              (cond
               ((file-directory-p path)
                (when (and (< depth pi-ui-fallback-file-scan-max-depth)
                           (file-readable-p path)
                           (not (pi-ui--prompt-fallback-ignore-directory-p name)))
                  (push (cons path (1+ depth)) queue)))
               ((file-regular-p path)
                (push (file-relative-name path root) results)
                (setq count (1+ count)))))))))
    (nreverse results)))

(defun pi-ui--prompt-file-candidates (&optional source-buffer)
  (let ((source-buffer (or source-buffer pi-ui--prompt-source-buffer)))
    (or pi-ui--prompt-file-cache
        (setq-local
         pi-ui--prompt-file-cache
         (let* ((root (file-name-as-directory (expand-file-name (pi-ui--prompt-source-root source-buffer))))
                (project (and (buffer-live-p source-buffer)
                              (with-current-buffer source-buffer
                                (project-current nil))))
                (project-files-list (when project
                                      (ignore-errors (project-files project))))
                (files (or project-files-list
                           (pi-ui--prompt-fallback-file-candidates root))))
           (sort (delete-dups
                  (seq-filter
                   (lambda (file)
                     (and (stringp file)
                          (not (string-empty-p file))
                          (not (string-match-p "\\`\\.git/" file))))
                   files))
                 #'string-lessp))))))

(defun pi-ui--prompt-file-reference-bounds ()
  (let* ((end (point))
         (start (save-excursion
                  (skip-chars-backward "^ \t\n\"'`()[]{}<>")
                  (point))))
    (when (and (< start end)
               (eq (char-after start) ?@))
      (cons (1+ start) end))))

(defun pi-ui--prompt-file-reference-capf ()
  (when-let* ((bounds (pi-ui--prompt-file-reference-bounds))
              (candidates (pi-ui--prompt-file-candidates)))
    (list (car bounds) (cdr bounds) candidates
          :annotation-function (lambda (_candidate) " [file]")
          :exclusive 'no)))

(defun pi-ui--prompt-command-bounds ()
  (let* ((end (point))
         (start (save-excursion
                  (skip-chars-backward "^ \t\n\"'`()[]{}<>")
                  (point))))
    (when (and (< start end)
               (eq (char-after start) ?/))
      (cons (1+ start) end))))

(defun pi-ui--prompt-command-candidates ()
  (or pi-ui--prompt-command-cache '()))

(defun pi-ui--prompt-command-annotation (candidate)
  (or (cdr (assoc candidate pi-ui--prompt-command-metadata)) " [command]"))

(defun pi-ui--prompt-prefetch-commands (prompt-buffer source-buffer)
  (when (and (buffer-live-p prompt-buffer)
             (buffer-live-p source-buffer))
    (with-current-buffer prompt-buffer
      (setq-local pi-ui--prompt-command-loading t))
    (let ((session (pi-session-ensure-for-buffer source-buffer)))
      (pi-session-get-commands
       session
       (lambda (_session response)
         (when (buffer-live-p prompt-buffer)
           (with-current-buffer prompt-buffer
             (setq-local pi-ui--prompt-command-loading nil)
             (if (eq (plist-get response :success) :json-false)
                 (message "pi: %s" (or (plist-get response :error)
                                       "Failed to load /commands"))
               (let ((commands (plist-get (plist-get response :data) :commands))
                     (names nil)
                     (meta nil))
                 (dolist (command commands)
                   (when-let* ((name (plist-get command :name)))
                     (push name names)
                     (push (cons name
                                 (format " [%s]"
                                         (or (plist-get command :source) "command")))
                           meta)))
                 (setq-local pi-ui--prompt-command-cache (sort (delete-dups names) #'string-lessp)
                             pi-ui--prompt-command-metadata meta))))))))))

(defun pi-ui--prompt-command-capf ()
  (when-let* ((bounds (pi-ui--prompt-command-bounds)))
    (unless (or pi-ui--prompt-command-cache pi-ui--prompt-command-loading)
      (pi-ui--prompt-prefetch-commands (current-buffer) pi-ui--prompt-source-buffer))
    (when-let* ((candidates (pi-ui--prompt-command-candidates)))
      (list (car bounds) (cdr bounds) candidates
            :annotation-function #'pi-ui--prompt-command-annotation
            :exclusive 'no))))

(defun pi-ui--prompt-completion-at-point ()
  (or (pi-ui--prompt-command-capf)
      (pi-ui--prompt-file-reference-capf)))

(defun pi-ui-prompt-insert-command ()
  "Insert a /command with completion."
  (interactive)
  (unless (derived-mode-p 'pi-prompt-buffer-mode)
    (user-error "Not in a pi prompt buffer"))
  (unless (or pi-ui--prompt-command-cache pi-ui--prompt-command-loading)
    (pi-ui--prompt-prefetch-commands (current-buffer) pi-ui--prompt-source-buffer))
  (when pi-ui--prompt-command-loading
    (user-error "Loading /commands, try again in a moment"))
  (let* ((commands (pi-ui--prompt-command-candidates))
         (choice (completing-read "Slash command: " commands nil t))
         (bounds (pi-ui--prompt-command-bounds)))
    (when (string-empty-p choice)
      (user-error "No command selected"))
    (if bounds
        (progn
          (delete-region (car bounds) (cdr bounds))
          (insert choice))
      (insert "/" choice))))

(defun pi-ui-prompt-insert-file-reference ()
  "Insert an @file reference with completion."
  (interactive)
  (unless (derived-mode-p 'pi-prompt-buffer-mode)
    (user-error "Not in a pi prompt buffer"))
  (let* ((files (pi-ui--prompt-file-candidates))
         (choice (completing-read "File reference: " files nil t))
         (bounds (pi-ui--prompt-file-reference-bounds)))
    (when (string-empty-p choice)
      (user-error "No file selected"))
    (if bounds
        (progn
          (delete-region (car bounds) (cdr bounds))
          (insert choice))
      (insert "@" choice))))

(defun pi-ui-prompt-cancel ()
  "Cancel prompt composition and close the prompt buffer."
  (interactive)
  (quit-window t))

(defun pi-ui-prompt-submit ()
  "Submit the current prompt buffer content."
  (interactive)
  (unless (derived-mode-p 'pi-prompt-buffer-mode)
    (user-error "Not in a pi prompt buffer"))
  (let* ((prompt-buffer (current-buffer))
         (prompt-window (get-buffer-window prompt-buffer t))
         (source-buffer pi-ui--prompt-source-buffer)
         (target-window (or (and (buffer-live-p source-buffer)
                                 (get-buffer-window source-buffer t))
                            pi-ui--prompt-source-window))
         (prompt (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (unless (buffer-live-p source-buffer)
      (user-error "Original source buffer is no longer available"))
    (when (string-empty-p prompt)
      (user-error "Prompt is empty"))
    (when (window-live-p target-window)
      (select-window target-window))
    (pi-ui-send-prompt source-buffer prompt)
    (when (window-live-p prompt-window)
      (quit-window t prompt-window))))

(defun pi-ui-compose-prompt (&optional source-buffer initial-text)
  "Open a prompt composer for SOURCE-BUFFER.
INITIAL-TEXT pre-fills the prompt buffer."
  (interactive)
  (let* ((source-buffer
          (cond
           ((buffer-live-p source-buffer) source-buffer)
           ((and (derived-mode-p 'pi-session-buffer-mode)
                 (buffer-live-p pi-ui--source-buffer))
            pi-ui--source-buffer)
           (t (current-buffer))))
         (source-window (selected-window))
         (context-block (pi-ui--capture-region-context source-buffer))
         (buffer (get-buffer-create (pi-ui--prompt-buffer-name source-buffer))))
    (with-current-buffer buffer
      (pi-prompt-buffer-mode)
      (setq-local pi-ui--prompt-source-buffer source-buffer)
      (setq-local pi-ui--prompt-source-window source-window)
      (setq-local pi-ui--prompt-file-cache nil)
      (setq-local pi-ui--prompt-command-cache nil)
      (setq-local pi-ui--prompt-command-metadata nil)
      (setq-local pi-ui--prompt-command-loading nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when (and (stringp initial-text)
                   (not (string-empty-p initial-text)))
          (insert initial-text))
        (when (and (stringp context-block)
                   (not (string-empty-p context-block)))
          (insert "\n\n" context-block))
        (goto-char (point-min))))
    (pi-ui--prompt-prefetch-commands buffer source-buffer)
    (let ((window (pi-ui--display-prompt-buffer buffer)))
      (when (window-live-p window)
        (select-window window))
      buffer)))

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

(if (fboundp 'markdown-mode)
    (define-derived-mode pi-session-buffer-mode markdown-mode "Pi-Session"
      "Major mode for pi session buffers."
      (setq buffer-read-only t)
      (setq-local truncate-lines nil)
      (setq-local pi-ui--session nil)
      (setq-local pi-ui--source-buffer nil)
      (setq-local pi-ui--history nil)
      (setq-local pi-ui--live-message nil)
      (setq-local pi-ui--transient-items nil)
      (setq-local pi-ui--loading nil)
      (setq-local pi-ui--pending-render-timer nil)
      (setq-local header-line-format nil)
      (add-hook 'kill-buffer-hook #'pi-ui--session-buffer-killed nil t))
  (define-derived-mode pi-session-buffer-mode special-mode "Pi-Session"
    "Major mode for pi session buffers."
    (setq-local truncate-lines nil)
    (setq-local pi-ui--session nil)
    (setq-local pi-ui--source-buffer nil)
    (setq-local pi-ui--history nil)
    (setq-local pi-ui--live-message nil)
    (setq-local pi-ui--transient-items nil)
    (setq-local pi-ui--loading nil)
    (setq-local pi-ui--pending-render-timer nil)
    (setq-local header-line-format nil)
    (add-hook 'kill-buffer-hook #'pi-ui--session-buffer-killed nil t)))

(defun pi-ui--session-buffer-name (session)
  (format "*pi:%s*" (pi-session-name session)))

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
  (display-buffer-in-side-window
   buffer
   `((side . ,pi-ui-window-side)
     ,@(if (memq pi-ui-window-side '(left right))
           `((window-width . ,pi-ui-window-size))
         `((window-height . ,pi-ui-window-size))))))

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
         (result-error (and result (or (plist-get result :error)
                                       (eq (plist-get result :isError) t)
                                       (eq (plist-get result :success) :json-false)))))
    (cond
     ((or (plist-get event :error) result-error) 'error)
     (result 'success)
     (t 'unknown))))

(defun pi-ui--tool-status-from-message (message)
  (cond
   ((or (plist-get message :error)
        (eq (plist-get message :isError) t)
        (eq (plist-get message :success) :json-false))
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
                           (_ nil)))
            (duration (when-let* ((formatted (pi-ui--format-duration-ms duration-ms)))
                        (propertize (format "(%s)" formatted) 'face 'pi-ui-tool-line-face))))
       (concat (propertize "Tool · " 'face 'pi-ui-tool-prefix-face)
               (propertize tool 'face (pi-ui--tool-summary-face status))
               (or detail-text "")
               (if status-mark (concat " " status-mark) "")
               (if duration (concat " " duration) "")
               "\n")))))

(defun pi-ui--section-label (text face &optional level)
  (let ((level (max 1 (or level 3))))
    (propertize (format "%s %s\n\n" (make-string level ?#) text)
                'face face)))

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
    ('tool-result
     (pi-ui--render-tool-result
      (plist-get item :tool-name)
      (or (plist-get item :text) "")
      (plist-get item :status)
      (plist-get item :duration-ms)
      (pi-ui--tool-detail-from-item item)))
    (_ "")))

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
      (insert (propertize (format "pi session: %s\n" (pi-session-name session))
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

(defun pi-ui--normalize-message-content (content)
  (cond
   ((listp content) content)
   ((stringp content)
    (if (string-empty-p content)
        nil
      (list (list :type "text" :text content))))
   (t nil)))

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
  (let ((buffer (pi-ui--get-buffer session)))
    (when (buffer-live-p buffer)
      (pcase (plist-get event :type)
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
         (pcase (plist-get event :method)
           ("notify"
            (pi-ui--append-transient
             buffer
             (list :kind 'notify :text (or (plist-get event :message) ""))))
           (_ nil)))
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

(unless pi-session-keepalive-predicate
  (setq pi-session-keepalive-predicate #'pi-ui--session-visible-p))

(add-hook 'pi-session-event-hook #'pi-ui--handle-session-event)

(provide 'pi-ui)

;;; pi-ui.el ends here
