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
(declare-function pi-ui-compose-prompt "pi-ui" (&optional source-buffer initial-text))

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

(defun pi-prompt ()
  "Compose and send a prompt to the current buffer scope session."
  (interactive)
  (pi-ui-compose-prompt (current-buffer)))

(provide 'pi)

;;; pi.el ends here
