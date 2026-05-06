;;; pi-ui.el --- Session response buffer UI for pi -*- lexical-binding: t; -*-

;; Author: Fangyuan and contributors
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Shared UI configuration plus prompt/session buffer modules for pi.

;;; Code:

(require 'pi-session)

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

(defcustom pi-ui-follow-current-buffer t
  "Whether visible pi session windows follow the selected source buffer.

When non-nil, changing the selected non-pi buffer switches an already visible
pi session window on the same frame to that buffer's existing scope session,
if one exists. This never creates a new pi session."
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

(require 'pi-prompt)
(require 'pi-render)
(require 'pi-extension-ui)
(require 'pi-session-buffer)

(unless pi-session-keepalive-predicate
  (setq pi-session-keepalive-predicate #'pi-ui--session-visible-p))

(add-hook 'pi-session-event-hook #'pi-ui--handle-session-event)

(provide 'pi-ui)

;;; pi-ui.el ends here
