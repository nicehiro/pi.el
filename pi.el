;;; pi.el --- Emacs frontend for pi -*- lexical-binding: t; -*-

;; Author: Fangyuan and contributors
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Terminal-free Emacs integration for pi.

;;; Code:

(require 'pi-rpc)
(require 'pi-session)
(require 'pi-ui)
(require 'seq)
(require 'subr-x)

(declare-function pi-rpc-json-truthy-p "pi-rpc" (value))
(declare-function pi-rpc-success-p "pi-rpc" (response))

(declare-function pi-ui-open-session "pi-ui" (&optional source-buffer))
(declare-function pi-ui-show-session-buffer "pi-ui" (&optional session source-buffer))
(declare-function pi-ui-toggle-session-buffer "pi-ui" (session &optional source-buffer))
(declare-function pi-ui-send-prompt "pi-ui" (source-buffer prompt))
(declare-function pi-ui-compose-prompt "pi-ui" (&optional source-buffer initial-text))

(declare-function pi-session-current-for-buffer "pi-session" (&optional buffer))
(declare-function pi-session-new-session "pi-session" (session &optional callback))
(declare-function pi-session-resume "pi-session" (session session-file &optional callback))
(declare-function pi-session-saved-sessions-for-buffer "pi-session" (&optional buffer))
(declare-function pi-session-tree-nodes "pi-session" (session))
(declare-function pi-session-navigate-tree "pi-session" (session entry-id callback))
(declare-function pi-session-get-fork-messages "pi-session" (session callback))
(declare-function pi-session-fork "pi-session" (session entry-id &optional callback))
(declare-function pi-session-clone "pi-session" (session &optional callback))
(declare-function pi-session-get-available-models "pi-session" (session callback))
(declare-function pi-session-compact "pi-session" (session &optional custom-instructions callback))
(declare-function pi-session-set-steering-mode "pi-session" (session mode &optional callback))
(declare-function pi-session-set-follow-up-mode "pi-session" (session mode &optional callback))
(declare-function pi-session-set-auto-compaction "pi-session" (session enabled &optional callback))
(declare-function pi-session-set-auto-retry "pi-session" (session enabled &optional callback))
(declare-function pi-session-abort-retry "pi-session" (session &optional callback))
(declare-function pi-session-get-session-stats "pi-session" (session callback))
(declare-function pi-session-get-last-assistant-text "pi-session" (session callback))
(declare-function pi-session-export-html "pi-session" (session &optional output-path callback))
(declare-function pi-session-set-model "pi-session" (session provider model-id &optional callback))
(declare-function pi-session-cycle-model "pi-session" (session &optional callback))
(declare-function pi-session-set-thinking-level "pi-session" (session level &optional callback))
(declare-function pi-session-cycle-thinking-level "pi-session" (session &optional callback))
(declare-function pi-session-send-steer "pi-session" (session message &optional callback))
(declare-function pi-session-send-follow-up "pi-session" (session message &optional callback))
(declare-function pi-session-restart "pi-session" (session))

(defvar pi-ui--session)
(defvar pi-ui--source-buffer)

(defgroup pi nil
  "Emacs integration for pi."
  :group 'applications)

(defconst pi-thinking-levels
  '("off" "minimal" "low" "medium" "high" "xhigh")
  "Display order for pi thinking levels.")

(defconst pi-queue-modes '("all" "one-at-a-time")
  "Current pi RPC queue modes.")

(defun pi--thinking-level-supported-p (model level)
  "Return non-nil when MODEL supports thinking LEVEL."
  (let* ((level-map (plist-get model :thinkingLevelMap))
         (key (intern (concat ":" level)))
         (value (and (listp level-map)
                     (plist-member level-map key)
                     (plist-get level-map key))))
    (and value (not (eq value :json-false)))))

(defun pi--supported-thinking-levels (model)
  "Return current pi thinking levels supported by MODEL."
  (seq-filter (lambda (level)
                (pi--thinking-level-supported-p model level))
              pi-thinking-levels))

(defvar pi-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'pi)
    (define-key map (kbd "p") #'pi-prompt)
    (define-key map (kbd "S") #'pi-steer)
    (define-key map (kbd "F") #'pi-follow-up)
    (define-key map (kbd "n") #'pi-new-session)
    (define-key map (kbd "R") #'pi-resume-session)
    (define-key map (kbd "f") #'pi-fork-session)
    (define-key map (kbd "C") #'pi-clone-session)
    (define-key map (kbd "c") #'pi-compact-session)
    (define-key map (kbd "A") #'pi-toggle-auto-compaction)
    (define-key map (kbd "M-r") #'pi-toggle-auto-retry)
    (define-key map (kbd "M-a") #'pi-abort-retry)
    (define-key map (kbd "a") #'pi-abort)
    (define-key map (kbd "e") #'pi-export-session-html)
    (define-key map (kbd "j") #'pi-tree)
    (define-key map (kbd "m") #'pi-cycle-model)
    (define-key map (kbd "s") #'pi-set-model)
    (define-key map (kbd "t") #'pi-cycle-thinking-level)
    (define-key map (kbd "T") #'pi-set-thinking-level)
    (define-key map (kbd "r") #'pi-reload-session)
    (define-key map (kbd "l") #'pi-list-sessions)
    map)
  "Prefix keymap for pi commands.")

(defun pi ()
  "Open the current buffer scope session.

If the scope has a saved active session, resume it automatically. Otherwise
create a new session for the scope."
  (interactive)
  (pi-ui-open-session (current-buffer)))

(defalias 'pi--rpc-success-p #'pi-rpc-success-p)

(defun pi--ensure-session (source-buffer &optional session-file)
  (let ((session (pi-session-ensure-for-buffer source-buffer session-file)))
    (pi-ui-show-session-buffer session source-buffer)
    session))

(defun pi--source-buffer ()
  "Return the user source buffer for current pi commands."
  (if (and (derived-mode-p 'pi-session-buffer-mode)
           (buffer-live-p pi-ui--source-buffer))
      pi-ui--source-buffer
    (current-buffer)))

(defun pi--model-ref (model)
  (when model
    (let ((provider (plist-get model :provider))
          (id (plist-get model :id)))
      (cond
       ((and provider id) (format "%s/%s" provider id))
       (id id)))))

(defun pi--model-name (model)
  (or (and model (plist-get model :name))
      (pi--model-ref model)
      "unknown model"))

(defun pi--model-candidate (model)
  (let ((name (pi--model-name model))
        (ref (pi--model-ref model)))
    (if (and ref (not (string= name ref)))
        (format "%s — %s" name ref)
      name)))

(defun pi--saved-session-label (saved-session)
  (let* ((path (plist-get saved-session :path))
         (name (plist-get saved-session :name))
         (preview (plist-get saved-session :preview))
         (session-id (or (plist-get saved-session :session-id)
                         (file-name-base (or path ""))))
         (short-id (if (and (stringp session-id)
                            (> (length session-id) 8))
                       (substring session-id 0 8)
                     session-id))
         (modified (plist-get saved-session :modified))
         (title (or (and (stringp name)
                         (not (string-empty-p name))
                         name)
                    (and (stringp preview)
                         (not (string-empty-p preview))
                         (replace-regexp-in-string "[\n\r\t ]+" " " preview))
                    (and path (file-name-base path))
                    "session")))
    (format "%s — %s [%s]"
            title
            (if (and modified (listp modified))
                (format-time-string "%Y-%m-%d %H:%M" modified)
              "unknown time")
            short-id)))

(defun pi--tree-source-buffer ()
  (pi--source-buffer))

(defun pi-toggle-window ()
  "Toggle the side pi window for the current project.

This command does not create a new pi session. If the current project or
directory scope has no existing pi session, signal a user error."
  (interactive)
  (let* ((source-buffer (pi--tree-source-buffer))
         (session (or (and (derived-mode-p 'pi-session-buffer-mode)
                           (bound-and-true-p pi-ui--session))
                      (pi-session-current-for-buffer source-buffer))))
    (pi-ui-toggle-session-buffer session source-buffer)))

(define-key pi-command-map (kbd "w") #'pi-toggle-window)

(defun pi--existing-session-for-tree (&optional source-buffer)
  (or (and (derived-mode-p 'pi-session-buffer-mode)
           (bound-and-true-p pi-ui--session))
      (pi-session-current-for-buffer source-buffer)
      (user-error "pi: no existing session for this scope")))

(defun pi--tree-node-reeditable-p (node)
  (let* ((entry (plist-get node :entry))
         (type (plist-get entry :type)))
    (or (equal type "custom_message")
        (and (equal type "message")
             (equal (plist-get (plist-get entry :message) :role) "user")))))

(defun pi-tree ()
  "Open a picker for the current session tree and switch to the selected point."
  (interactive)
  (let* ((source-buffer (pi--tree-source-buffer))
         (session (pi--existing-session-for-tree source-buffer))
         (nodes (pi-session-tree-nodes session)))
    (unless nodes
      (user-error "pi: current session tree is empty"))
    (let* ((choices (mapcar (lambda (node)
                              (cons (plist-get node :display) node))
                            nodes))
           (default-choice (car (seq-find (lambda (choice)
                                            (plist-get (cdr choice) :current))
                                          choices)))
           (selection (let ((completion-extra-properties
                             '(:display-sort-function identity
                               :cycle-sort-function identity)))
                        (completing-read "Session tree: "
                                         (mapcar #'car choices)
                                         nil t nil nil default-choice)))
           (node (cdr (assoc selection choices))))
      (when node
        (if (and (plist-get node :current)
                 (not (pi--tree-node-reeditable-p node)))
            (message "pi: already at this point")
          (pi-session-navigate-tree
           session
           (plist-get node :id)
           (lambda (s response)
             (if (pi--rpc-success-p response)
                 (let ((editor-text (plist-get (plist-get response :data) :editor-text)))
                   (pi-ui-show-session-buffer s source-buffer)
                   (if (and (stringp editor-text)
                            (not (string-empty-p editor-text)))
                       (progn
                         (message "pi: rewound session; edit the prompt to continue")
                         (pi-ui-compose-prompt source-buffer editor-text))
                     (message "pi: moved to selected tree entry")))
               (message "pi: %s"
                        (or (plist-get response :error)
                            "Failed to switch to the selected tree entry"))))))))))

(defun pi-prompt ()
  "Compose and send a prompt to the current buffer scope session."
  (interactive)
  (pi-ui-compose-prompt (current-buffer)))

(defun pi--read-message (prompt)
  (let ((message (read-string prompt)))
    (when (string-empty-p (string-trim message))
      (user-error "pi: empty message"))
    message))

(defun pi-steer (message)
  "Queue MESSAGE as a steering instruction for the current session."
  (interactive (list (pi--read-message "Steer pi: ")))
  (let* ((source-buffer (pi--source-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-send-steer
     session message
     (lambda (_s response)
       (if (pi--rpc-success-p response)
           (message "pi: queued steering message")
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to queue steering message")))))))

(defun pi-follow-up (message)
  "Queue MESSAGE as a follow-up for the current session."
  (interactive (list (pi--read-message "Follow up after pi finishes: ")))
  (let* ((source-buffer (pi--source-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-send-follow-up
     session message
     (lambda (_s response)
       (if (pi--rpc-success-p response)
           (message "pi: queued follow-up message")
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to queue follow-up message")))))))

(defun pi-resume-session ()
  "Select and resume a saved session for the current buffer scope."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (saved-sessions (pi-session-saved-sessions-for-buffer source-buffer)))
    (if (null saved-sessions)
        (message "pi: no saved sessions for this project")
      (let* ((choices (mapcar (lambda (saved-session)
                                (cons (pi--saved-session-label saved-session)
                                      saved-session))
                              saved-sessions))
             (current-session (pi-session-current-for-buffer source-buffer))
             (current-file (and current-session
                                (pi-session-session-file current-session)))
             (default-choice
              (car (seq-find (lambda (entry)
                               (equal (plist-get (cdr entry) :path)
                                      current-file))
                             choices)))
             (selection (let ((completion-extra-properties
                               '(:display-sort-function identity
                                 :cycle-sort-function identity)))
                          (completing-read
                           "Resume session: "
                           (mapcar #'car choices)
                           nil t nil nil default-choice)))
             (saved-session (cdr (assoc selection choices)))
             (session-file (plist-get saved-session :path))
             (session (pi--ensure-session source-buffer session-file)))
        (pi-session-resume
         session session-file
         (lambda (s response)
           (if (pi--rpc-success-p response)
               (progn
                 (pi-ui-show-session-buffer s source-buffer)
                 (message "pi: resumed session"))
             (message "pi: %s"
                      (or (plist-get response :error)
                          "Failed to resume session")))))))))

(defun pi-new-session ()
  "Start a fresh session for the current buffer scope."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-new-session
     session
     (lambda (s response)
       (if (pi--rpc-success-p response)
           (progn
             (pi-ui-show-session-buffer s source-buffer)
             (message "pi: started a new session"))
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to start a new session")))))))

(defun pi--fork-message-label (message)
  (let* ((entry-id (or (plist-get message :entryId) ""))
         (text (string-trim (or (plist-get message :text) "")))
         (preview (replace-regexp-in-string "[\n\r\t ]+" " " text))
         (short-id (substring entry-id 0 (min 8 (length entry-id)))))
    (format "user: %s [%s]" preview short-id)))

(defun pi-fork-session ()
  "Fork the current session from a previous user message.

The selected prompt is restored into the composer for editing, matching pi's
current RPC `fork' behavior."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-get-fork-messages
     session
     (lambda (s response)
       (if (not (pi--rpc-success-p response))
           (message "pi: %s" (or (plist-get response :error)
                                 "Failed to load fork messages"))
         (let* ((messages (plist-get (plist-get response :data) :messages))
                (choices (mapcar (lambda (message)
                                   (cons (pi--fork-message-label message) message))
                                 messages)))
           (if (null choices)
               (message "pi: no user messages available to fork")
             (let* ((selection (let ((completion-extra-properties
                                      '(:display-sort-function identity
                                        :cycle-sort-function identity)))
                                 (completing-read "Fork from message: "
                                                  (mapcar #'car choices)
                                                  nil t)))
                    (message-info (cdr (assoc selection choices)))
                    (entry-id (plist-get message-info :entryId)))
               (pi-session-fork
                s entry-id
                (lambda (forked response2)
                  (cond
                   ((not (pi--rpc-success-p response2))
                    (message "pi: %s" (or (plist-get response2 :error)
                                          "Failed to fork session")))
                   ((plist-get (plist-get response2 :data) :cancelled)
                    (message "pi: fork cancelled"))
                   (t
                    (let ((text (plist-get (plist-get response2 :data) :text)))
                      (pi-ui-show-session-buffer forked source-buffer)
                      (message "pi: forked session; edit the prompt to continue")
                      (when (and (stringp text) (not (string-empty-p text)))
                        (pi-ui-compose-prompt source-buffer text)))))))))))))))

(defun pi-clone-session ()
  "Duplicate the current active branch into a new session file."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-clone
     session
     (lambda (s response)
       (cond
        ((not (pi--rpc-success-p response))
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to clone session")))
        ((plist-get (plist-get response :data) :cancelled)
         (message "pi: clone cancelled"))
        (t
         (pi-ui-show-session-buffer s source-buffer)
         (message "pi: cloned current branch into a new session")))))))

(defun pi-compact-session (instructions)
  "Compact the current session context.
With optional INSTRUCTIONS, pass custom compaction guidance."
  (interactive (list (let ((input (read-string "Compact instructions (optional): ")))
                       (unless (string-empty-p input) input))))
  (let* ((source-buffer (current-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-compact
     session instructions
     (lambda (s response)
       (if (pi--rpc-success-p response)
           (progn
             (pi-ui-show-session-buffer s source-buffer)
             (message "pi: compacted session context"))
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to compact session")))))))

(defun pi-toggle-auto-compaction ()
  "Toggle automatic compaction for the current session."
  (interactive)
  (let* ((source-buffer (pi--source-buffer))
         (session (pi--ensure-session source-buffer))
         (enabled (not (pi-rpc-json-truthy-p
                        (plist-get (pi-session-cached-state session)
                                   :auto-compaction-enabled)))))
    (pi-session-set-auto-compaction
     session enabled
     (lambda (_s response)
       (if (pi--rpc-success-p response)
           (message "pi: auto-compaction %s" (if enabled "enabled" "disabled"))
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to update auto-compaction")))))))

(defun pi--read-queue-mode (session state-key label)
  (let ((current (plist-get (pi-session-cached-state session) state-key)))
    (completing-read
     (if current
         (format "%s mode (current %s): " label current)
       (format "%s mode: " label))
     pi-queue-modes nil t nil nil
     (and (member current pi-queue-modes) current))))

(defun pi-set-steering-mode (mode)
  "Set how queued steering messages are delivered."
  (interactive
   (let* ((source-buffer (pi--source-buffer))
          (session (pi--ensure-session source-buffer)))
     (list (pi--read-queue-mode session :steering-mode "Steering"))))
  (let* ((source-buffer (pi--source-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-set-steering-mode
     session mode
     (lambda (_s response)
       (if (pi--rpc-success-p response)
           (message "pi: steering mode set to %s" mode)
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to set steering mode")))))))

(defun pi-set-follow-up-mode (mode)
  "Set how queued follow-up messages are delivered."
  (interactive
   (let* ((source-buffer (pi--source-buffer))
          (session (pi--ensure-session source-buffer)))
     (list (pi--read-queue-mode session :follow-up-mode "Follow-up"))))
  (let* ((source-buffer (pi--source-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-set-follow-up-mode
     session mode
     (lambda (_s response)
       (if (pi--rpc-success-p response)
           (message "pi: follow-up mode set to %s" mode)
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to set follow-up mode")))))))

(defun pi-set-auto-retry (enabled)
  "Enable or disable automatic retry for the current session."
  (interactive
   (let* ((source-buffer (pi--source-buffer))
          (session (pi--ensure-session source-buffer))
          (current (pi-rpc-json-truthy-p
                    (plist-get (pi-session-cached-state session)
                               :auto-retry-enabled))))
     (list (y-or-n-p (format "Enable auto-retry? (currently %s) "
                             (if current "enabled" "disabled"))))))
  (let* ((source-buffer (pi--source-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-set-auto-retry
     session enabled
     (lambda (_s response)
       (if (pi--rpc-success-p response)
           (message "pi: auto-retry %s" (if enabled "enabled" "disabled"))
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to update auto-retry")))))))

(defun pi-toggle-auto-retry ()
  "Toggle automatic retry for the current session."
  (interactive)
  (let* ((source-buffer (pi--source-buffer))
         (session (pi--ensure-session source-buffer))
         (enabled (not (pi-rpc-json-truthy-p
                        (plist-get (pi-session-cached-state session)
                                   :auto-retry-enabled)))))
    (pi-session-set-auto-retry
     session enabled
     (lambda (_s response)
       (if (pi--rpc-success-p response)
           (message "pi: auto-retry %s" (if enabled "enabled" "disabled"))
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to update auto-retry")))))))

(defun pi-abort-retry ()
  "Abort the current automatic retry delay."
  (interactive)
  (let* ((source-buffer (pi--source-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-abort-retry
     session
     (lambda (_s response)
       (if (pi--rpc-success-p response)
           (message "pi: aborted auto-retry")
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to abort auto-retry")))))))

(defun pi-abort ()
  "Abort the current run for the active session."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-abort
     session
     (lambda (_s response)
       (if (pi--rpc-success-p response)
           (message "pi: aborted current run")
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to abort")))))))

(defun pi--format-number (value)
  (if (numberp value)
      (format "%s" value)
    "?"))

(defun pi--format-session-stats (stats)
  (let* ((tokens (plist-get stats :tokens))
         (context (plist-get stats :contextUsage))
         (parts (list (format "%s messages" (pi--format-number (plist-get stats :totalMessages)))
                      (format "%s tool calls" (pi--format-number (plist-get stats :toolCalls))))))
    (when-let* ((total (plist-get tokens :total)))
      (setq parts (append parts (list (format "%s tokens" total)))))
    (when-let* ((cost (plist-get stats :cost)))
      (setq parts (append parts (list (format "$%.4f" cost)))))
    (when context
      (let ((percent (plist-get context :percent))
            (used (plist-get context :tokens))
            (window (plist-get context :contextWindow)))
        (setq parts
              (append parts
                      (list (format "context %s/%s%s"
                                    (pi--format-number used)
                                    (pi--format-number window)
                                    (if (numberp percent)
                                        (format " %.1f%%" percent)
                                      "")))))))
    (string-join parts " • ")))

(defun pi-session-stats ()
  "Show concise token/cost/context stats for the current session."
  (interactive)
  (let* ((source-buffer (pi--source-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-get-session-stats
     session
     (lambda (_s response)
       (if (pi--rpc-success-p response)
           (message "pi: %s" (pi--format-session-stats (plist-get response :data)))
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to load session stats")))))))

(defun pi-last-assistant-text ()
  "Copy the current session's last assistant text to the kill ring."
  (interactive)
  (let* ((source-buffer (pi--source-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-get-last-assistant-text
     session
     (lambda (_s response)
       (if (pi--rpc-success-p response)
           (let ((text (plist-get (plist-get response :data) :text)))
             (if (and (stringp text) (not (string-empty-p text)))
                 (progn
                   (kill-new text)
                   (message "pi: copied last assistant text"))
               (message "pi: no assistant text in this session")))
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to load last assistant text")))))))

(defun pi-export-session-html (path)
  "Export current session to HTML.
If PATH is empty, let pi choose the output path."
  (interactive (list (let ((input (read-string "Export HTML path (optional): ")))
                       (unless (string-empty-p input) (expand-file-name input)))))
  (let* ((source-buffer (current-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-export-html
     session path
     (lambda (_s response)
       (if (pi--rpc-success-p response)
           (let ((actual (plist-get (plist-get response :data) :path)))
             (message "pi: exported session to %s" (or actual path "HTML file")))
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to export session")))))))

(defun pi-cycle-model ()
  "Cycle to the next configured model for the current session."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-cycle-model
     session
     (lambda (_s response)
       (if (pi--rpc-success-p response)
           (let* ((data (plist-get response :data))
                  (model (and data (plist-get data :model))))
             (message "%s" (if model
                                (format "pi: switched model to %s" (pi--model-name model))
                              "pi: model unchanged")))
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to cycle model")))))))

(defun pi-set-model ()
  "Select a configured model for the current session."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-get-available-models
     session
     (lambda (s response)
       (if (not (pi--rpc-success-p response))
           (message "pi: %s" (or (plist-get response :error)
                                 "Failed to load models"))
         (let* ((models (plist-get (plist-get response :data) :models))
                (choices (mapcar (lambda (model)
                                   (cons (pi--model-candidate model) model))
                                 models))
                (current-model (plist-get (pi-session-cached-state s) :model))
                (current-ref (pi--model-ref current-model))
                (default (car (seq-find (lambda (entry)
                                          (equal (pi--model-ref (cdr entry)) current-ref))
                                        choices))))
           (if (null choices)
               (message "pi: no configured models available")
             (let* ((selection (completing-read
                                (if current-model
                                    (format "Model (current %s): "
                                            (pi--model-name current-model))
                                  "Model: ")
                                (mapcar #'car choices)
                                nil t nil nil default))
                    (model (cdr (assoc selection choices))))
               (when model
                 (pi-session-set-model
                  s
                  (plist-get model :provider)
                  (plist-get model :id)
                  (lambda (_s2 set-response)
                    (if (pi--rpc-success-p set-response)
                        (message "pi: switched model to %s"
                                 (pi--model-name (or (plist-get set-response :data)
                                                     model)))
                      (message "pi: %s" (or (plist-get set-response :error)
                                            "Failed to set model"))))))))))))))

(defun pi-cycle-thinking-level ()
  "Cycle to the next thinking level for the current session."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-cycle-thinking-level
     session
     (lambda (_s response)
       (if (pi--rpc-success-p response)
           (let* ((data (plist-get response :data))
                  (level (and data (plist-get data :level))))
             (message "%s" (if level
                                (format "pi: thinking level is now %s" level)
                              "pi: thinking level unchanged")))
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to cycle thinking level")))))))

(defun pi--read-thinking-level (session)
  "Read a thinking level supported by SESSION's current model."
  (let* ((state (pi-session-cached-state session))
         (model (plist-get state :model))
         (levels (pi--supported-thinking-levels model))
         (current (plist-get state :thinking-level)))
    (unless levels
      (user-error "pi: current model does not expose configurable thinking levels"))
    (completing-read
     (if current
         (format "Thinking level (current %s): " current)
       "Thinking level: ")
     levels nil t nil nil
     (and (member current levels) current))))

(defun pi-set-thinking-level (level)
  "Set thinking LEVEL for the current session."
  (interactive
   (let* ((source-buffer (pi--source-buffer))
          (session (pi--ensure-session source-buffer)))
     (list (pi--read-thinking-level session))))
  (let* ((source-buffer (pi--source-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-set-thinking-level
     session level
     (lambda (_s response)
       (if (pi--rpc-success-p response)
           (message "pi: set thinking level to %s" level)
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to set thinking level")))))))

(defun pi-reload-session ()
  "Restart the current scope session process."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (session (pi--ensure-session source-buffer)))
    (pi-session-restart session)
    (pi-ui-show-session-buffer session source-buffer)
    (message "pi: reloaded session")))

(provide 'pi)

;;; pi.el ends here
