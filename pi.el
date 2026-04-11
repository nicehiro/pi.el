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

(declare-function pi-ui-open-session "pi-ui" (&optional source-buffer))
(declare-function pi-ui-send-prompt "pi-ui" (source-buffer prompt))

(defgroup pi nil
  "Emacs integration for pi."
  :group 'applications)

(defvar pi-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'pi-open)
    (define-key map (kbd "p") #'pi-prompt)
    (define-key map (kbd "l") #'pi-list-sessions)
    map)
  "Prefix keymap for pi commands.")

(defun pi-open ()
  "Open and focus the current buffer scope session."
  (interactive)
  (pi-ui-open-session (current-buffer)))

(defun pi-prompt (prompt)
  "Send PROMPT to the current buffer scope session."
  (interactive (list (read-string "Pi prompt: ")))
  (pi-ui-send-prompt (current-buffer) prompt))

(provide 'pi)

;;; pi.el ends here
