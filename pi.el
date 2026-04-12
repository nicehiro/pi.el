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
(require 'subr-x)

(declare-function pi-ui-open-session "pi-ui" (&optional source-buffer))
(declare-function pi-ui-show-session-buffer "pi-ui" (&optional session source-buffer))
(declare-function pi-ui-send-prompt "pi-ui" (source-buffer prompt))
(declare-function pi-ui-compose-prompt "pi-ui" (&optional source-buffer initial-text))

(declare-function pi-session-new-session "pi-session" (session &optional callback))
(declare-function pi-session-compact "pi-session" (session &optional custom-instructions callback))
(declare-function pi-session-export-html "pi-session" (session &optional output-path callback))
(declare-function pi-session-cycle-model "pi-session" (session &optional callback))
(declare-function pi-session-set-thinking-level "pi-session" (session level &optional callback))
(declare-function pi-session-cycle-thinking-level "pi-session" (session &optional callback))
(declare-function pi-session-restart "pi-session" (session))

(defgroup pi nil
  "Emacs integration for pi."
  :group 'applications)

(defconst pi-thinking-levels
  '("off" "minimal" "low" "medium" "high" "xhigh")
  "Supported thinking levels for pi.")

(defvar pi-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'pi-open)
    (define-key map (kbd "p") #'pi-prompt)
    (define-key map (kbd "n") #'pi-new-session)
    (define-key map (kbd "c") #'pi-compact-session)
    (define-key map (kbd "a") #'pi-abort)
    (define-key map (kbd "e") #'pi-export-session-html)
    (define-key map (kbd "m") #'pi-cycle-model)
    (define-key map (kbd "t") #'pi-cycle-thinking-level)
    (define-key map (kbd "T") #'pi-set-thinking-level)
    (define-key map (kbd "r") #'pi-reload-session)
    (define-key map (kbd "l") #'pi-list-sessions)
    map)
  "Prefix keymap for pi commands.")

(defun pi-open ()
  "Open and focus the current buffer scope session."
  (interactive)
  (pi-ui-open-session (current-buffer)))

(defun pi--rpc-success-p (response)
  (not (eq (plist-get response :success) :json-false)))

(defun pi--ensure-session (source-buffer)
  (let ((session (pi-session-ensure-for-buffer source-buffer)))
    (pi-ui-show-session-buffer session source-buffer)
    session))

(defun pi-prompt ()
  "Compose and send a prompt to the current buffer scope session."
  (interactive)
  (pi-ui-compose-prompt (current-buffer)))

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
                  (model (and data (plist-get data :model)))
                  (name (or (and model (plist-get model :name))
                            (and model (plist-get model :id)))))
             (message "%s" (if name
                                (format "pi: switched model to %s" name)
                              "pi: model unchanged")))
         (message "pi: %s" (or (plist-get response :error)
                               "Failed to cycle model")))))))

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

(defun pi-set-thinking-level (level)
  "Set thinking LEVEL for the current session."
  (interactive (list (completing-read "Thinking level: " pi-thinking-levels nil t)))
  (let* ((source-buffer (current-buffer))
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
