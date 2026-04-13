;;; pi-prompt.el --- Prompt composer UI for pi -*- lexical-binding: t; -*-

;; Author: Fangyuan and contributors
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Prompt composition and completion helpers for pi.

;;; Code:

(require 'project)
(require 'seq)
(require 'subr-x)
(require 'pi-session)

(declare-function pi-ui-send-prompt "pi-session-buffer" (source-buffer prompt))

(defvar pi-ui-prompt-window-side)
(defvar pi-ui-prompt-window-size)
(defvar pi-ui-include-active-region)
(defvar pi-ui-region-context-max-chars)
(defvar pi-ui-fallback-file-scan-max-files)
(defvar pi-ui-fallback-file-scan-max-depth)
(defvar pi-ui--source-buffer)

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

(provide 'pi-prompt)

;;; pi-prompt.el ends here
