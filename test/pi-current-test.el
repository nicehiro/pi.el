;;; pi-current-test.el --- Current pi RPC integration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))

(require 'pi-rpc)
(require 'pi-session)
(require 'pi-session-buffer)
(require 'pi-render)
(require 'pi)

(ert-deftest pi-rpc-jsonl-splitting-strict-lf ()
  (let ((events nil)
        (responses nil)
        (process nil))
    (unwind-protect
        (let* ((rpc (pi-rpc--create
                     :pending-by-id (make-hash-table :test #'equal)
                     :event-handler (lambda (_rpc event) (push event events))
                     :stdout-fragment ""
                     :alive-p t)))
          (puthash "req-1" (lambda (response) (push response responses))
                   (pi-rpc-pending-by-id rpc))
          (setq process (make-process :name "pi-rpc-jsonl-test"
                                      :command '("cat")
                                      :noquery t
                                      :connection-type 'pipe))
          (process-put process 'pi-rpc-object rpc)
          (pi-rpc--process-filter
           process
           "{\"type\":\"event\",\"value\":1}\r\n{\"type\":\"response\",\"id\":\"req-1\",\"success\":true")
          (should (= (length events) 1))
          (should (equal (plist-get (car events) :value) 1))
          (should (null responses))
          (should (string-prefix-p "{\"type\":\"response\"" (pi-rpc-stdout-fragment rpc)))
          (pi-rpc--process-filter process ",\"command\":\"get_state\"}\n")
          (should (= (length responses) 1))
          (should (equal (plist-get (car responses) :command) "get_state"))
          (should (equal (pi-rpc-stdout-fragment rpc) "")))
      (when (process-live-p process)
        (delete-process process)))))

(ert-deftest pi-rpc-start-keeps-stderr-out-of-jsonl-filter ()
  (let (captured-args
        rpc)
    (let ((pi-rpc-debug-buffer t))
      (cl-letf (((symbol-function 'pi-rpc-check-version) #'ignore)
                ((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq captured-args args)
                   'fake-process))
                ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
                ((symbol-function 'process-put) #'ignore))
        (unwind-protect
            (progn
              (setq rpc (pi-rpc-start default-directory "stderr-test"))
              (should (buffer-live-p (plist-get captured-args :buffer)))
              (should (buffer-live-p (plist-get captured-args :stderr)))
              (should-not (eq (plist-get captured-args :stderr)
                              (plist-get captured-args :buffer)))
              (should (eq (pi-rpc-stderr-buffer rpc)
                          (plist-get captured-args :stderr))))
          (when (and rpc (buffer-live-p (pi-rpc-buffer rpc)))
            (kill-buffer (pi-rpc-buffer rpc)))
          (when (and rpc (buffer-live-p (pi-rpc-stderr-buffer rpc)))
            (kill-buffer (pi-rpc-stderr-buffer rpc))))))))

(ert-deftest pi-rpc-message-update-uses-current-assistant-event-shape ()
  (should (pi-ui--message-update-assistant-p
           '(:type "message_update"
             :message (:role "assistant" :content ((:type "text" :text "Hello")))
             :assistantMessageEvent (:type "text_delta" :delta "Hello"))))
  (should-not (pi-ui--message-update-assistant-p
               '(:type "message_update"
                 :role "assistant"
                 :delta "legacy"))))

(ert-deftest pi-rpc-tool-execution-update-uses-accumulated-partial-result ()
  (let ((item (pi-ui--tool-result-from-event
               '(:type "tool_execution_update"
                 :toolCallId "call-1"
                 :toolName "bash"
                 :partialResult (:content ((:type "text" :text "line 1\nline 2")))))))
    (should (eq (plist-get item :status) 'running))
    (should (equal (plist-get item :tool-call-id) "call-1"))
    (should (equal (plist-get item :text) "line 1\nline 2"))))

(ert-deftest pi-render-extension-state-strips-ansi-escapes ()
  (let ((rendered (pi-ui--render-extension-state
                   '(("mcp" . "\33[38;5;116mMCP: 0/2 servers\33[39m"))
                   nil
                   nil)))
    (should (string-match-p "mcp: MCP: 0/2 servers" rendered))
    (should-not (string-match-p "\\[38;5;116m" rendered))
    (should-not (string-match-p "\33" rendered))))

(ert-deftest pi-render-assistant-error-message-is-visible ()
  (let ((rendered (pi-ui--render-message
                   '(:role "assistant"
                     :content nil
                     :stopReason "error"
                     :errorMessage "stream_read_error"))))
    (should (string-match-p "Assistant" rendered))
    (should (string-match-p "Assistant error: stream_read_error" rendered))))

(ert-deftest pi-render-compaction-summary-includes-estimated-tokens-after ()
  (should (equal (pi-ui--compaction-result-summary
                  '(:tokensBefore 150000
                    :estimatedTokensAfter 32000
                    :firstKeptEntryId "abcdef123456"))
                 "150000 -> ~32000 tokens, kept from abcdef12")))

(ert-deftest pi-thinking-level-map-filters-supported-levels ()
  (let ((model '(:reasoning t
                 :thinkingLevelMap (:off t :minimal :json-false :low t
                                    :medium t :high :json-false))))
    (should (equal (pi--supported-thinking-levels model)
                   '("off" "low" "medium")))))

(ert-deftest pi-thinking-levels-default-when-map-absent ()
  (should (equal (pi--supported-thinking-levels '(:reasoning t))
                 '("off" "minimal" "low" "medium" "high"))))

(ert-deftest pi-thinking-levels-non-reasoning-model ()
  (dolist (model '((:reasoning :json-false :thinkingLevelMap (:max "max"))
                   (:reasoning nil :thinkingLevelMap (:xhigh "xhigh"))))
    (should (equal (pi--supported-thinking-levels model) '("off")))))

(ert-deftest pi-thinking-levels-explicit-xhigh-max ()
  (should (equal (pi--supported-thinking-levels
                  '(:reasoning t :thinkingLevelMap (:xhigh "xhigh" :max "max")))
                 pi-thinking-levels)))

(ert-deftest pi-thinking-levels-null-disables-level ()
  (should (equal (pi--supported-thinking-levels
                  '(:reasoning t :thinkingLevelMap (:medium nil)))
                 '("off" "minimal" "low" "high"))))

(ert-deftest pi-session-source-buffer-prefers-recorded-source ()
  (let ((source (generate-new-buffer " *pi-source-test*"))
        (session-buffer (generate-new-buffer " *pi-session-buffer-test*")))
    (unwind-protect
        (with-current-buffer session-buffer
          (pi-session-buffer-mode)
          (setq-local pi-ui--source-buffer source)
          (should (eq (pi--source-buffer) source)))
      (when (buffer-live-p source)
        (kill-buffer source))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest pi-session-apply-state-normalizes-json-booleans ()
  (let ((session (pi-session--create
                  :id "test-session"
                  :cached-state '(:auto-retry-enabled t :is-retrying t))))
    (pi-session--apply-state
     session
     '(:success t
       :data (:model (:id "m")
              :thinkingLevel "off"
              :isStreaming :json-false
              :isCompacting :json-false
              :autoCompactionEnabled :json-false
              :sessionFile "/tmp/pi-test.jsonl"
              :sessionId "sid")))
    (should-not (plist-get (pi-session-cached-state session) :is-streaming))
    (should-not (plist-get (pi-session-cached-state session) :is-compacting))
    (should-not (plist-get (pi-session-cached-state session) :auto-compaction-enabled))
    (should (plist-get (pi-session-cached-state session) :auto-retry-enabled))
    (should (plist-get (pi-session-cached-state session) :is-retrying))))

(ert-deftest pi-session-apply-state-preserves-absent-local-auto-retry ()
  (let ((session (pi-session--create
                  :id "test-session"
                  :cached-state '(:auto-retry-enabled t :is-retrying t))))
    (pi-session--apply-state
     session
     '(:success t
       :data (:model (:id "m")
              :thinkingLevel "off"
              :isStreaming :json-false
              :isCompacting :json-false
              :sessionFile "/tmp/pi-test.jsonl"
              :sessionId "sid")))
    (should (eq (plist-get (pi-session-cached-state session) :auto-retry-enabled) t))
    (should (eq (plist-get (pi-session-cached-state session) :is-retrying) t))))

(ert-deftest pi-session-message-end-error-clears-streaming-state ()
  (let ((session (pi-session--create
                  :id "test-session"
                  :cached-state '(:is-streaming t))))
    (pi-session--apply-assistant-message-end
     session
     '(:type "message_end"
       :message (:role "assistant"
                 :stopReason "error"
                 :errorMessage "stream_read_error")))
    (should-not (plist-get (pi-session-cached-state session) :is-streaming))
    (should (equal (plist-get (pi-session-cached-state session) :last-error)
                   "stream_read_error"))))

(ert-deftest pi-toggle-auto-retry-defaults-to-enabled ()
  (let ((session (pi-session--create :id "test-session" :cached-state nil))
        captured)
    (cl-letf (((symbol-function 'pi--source-buffer) (lambda () (current-buffer)))
              ((symbol-function 'pi--ensure-session) (lambda (&rest _) session))
              ((symbol-function 'pi-session-set-auto-retry)
               (lambda (_session enabled &optional _callback)
                 (setq captured enabled))))
      (pi-toggle-auto-retry))
    (should-not captured)))

(ert-deftest pi-session-agent-end-keeps-streaming ()
  (let ((session (pi-session--create
                  :id "test-session" :cached-state '(:is-streaming t)))
        (pi-session-event-hook nil))
    (cl-letf (((symbol-function 'pi-session--touch) #'ignore))
      (pi-session--event-dispatch session '(:type "agent_end" :willRetry t)))
    (should (plist-get (pi-session-cached-state session) :is-streaming))))

(ert-deftest pi-session-agent-settled-clears-streaming-and-retry ()
  (let ((session (pi-session--create
                  :id "test-session"
                  :cached-state '(:is-streaming t :is-retrying t
                                  :retry-attempt 2 :retry-max-attempts 3)))
        (pi-session-event-hook nil))
    (cl-letf (((symbol-function 'pi-session--touch) #'ignore))
      (pi-session--event-dispatch session '(:type "agent_settled")))
    (let ((state (pi-session-cached-state session)))
      (should-not (plist-get state :is-streaming))
      (should-not (plist-get state :is-retrying))
      (should-not (plist-get state :retry-attempt))
      (should-not (plist-get state :retry-max-attempts)))))

(ert-deftest pi-session-message-end-stop-keeps-streaming ()
  (let ((session (pi-session--create
                  :id "test-session" :cached-state '(:is-streaming t))))
    (pi-session--apply-assistant-message-end
     session '(:type "message_end"
               :message (:role "assistant" :stopReason "stop")))
    (should (plist-get (pi-session-cached-state session) :is-streaming))))

(ert-deftest pi-session-resume-cancelled-does-not-set-active-file ()
  (let* ((requested (make-temp-file "pi-requested-" nil ".jsonl"))
         (actual (make-temp-file "pi-actual-" nil ".jsonl"))
         (index-file (make-temp-file "pi-index-"))
         (pi-session-active-index-file index-file)
         (pi-session--active-index (make-hash-table :test #'equal))
         (session (pi-session--create
                   :id "test-session" :scope 'directory :scope-key "scope"
                   :root default-directory :session-file actual :rpc 'fake-rpc
                   :status 'ready :cached-state nil))
         callback-response)
    (unwind-protect
        (cl-letf (((symbol-function 'pi-rpc-live-p) (lambda (_rpc) t))
                  ((symbol-function 'pi-session--touch) #'ignore)
                  ((symbol-function 'pi-rpc-send)
                   (lambda (_rpc command callback)
                     (pcase (cdr (assoc "type" command))
                       ("get_state"
                        (funcall callback
                                 `(:success t :data (:sessionFile ,actual
                                                :sessionId "actual"))))
                       ("switch_session"
                        (funcall callback '(:success t :data (:cancelled t))))))))
          (pi-session-resume
           session requested
           (lambda (_session response) (setq callback-response response))))
      (delete-file requested)
      (delete-file actual)
      (delete-file index-file))
    (should (plist-get (plist-get callback-response :data) :cancelled))
    (should (equal (pi-session-session-file session) actual))
    (should-not (equal (gethash "scope" pi-session--active-index) requested))))

(ert-deftest pi-session-bootstrap-cancelled-switch-marks-ready ()
  (let* ((requested "/tmp/pi-requested.jsonl")
         (actual "/tmp/pi-actual.jsonl")
         (pi-session-active-index-file (make-temp-file "pi-index-"))
         (pi-session--active-index (make-hash-table :test #'equal))
         (events nil)
         (pi-session-event-hook
          (list (lambda (_session event) (push event events))))
         (session (pi-session--create
                   :id "test-session" :scope 'directory :scope-key "scope"
                   :root default-directory :session-file requested :rpc 'fake-rpc
                   :status 'starting :cached-state nil)))
    (unwind-protect
        (cl-letf (((symbol-function 'pi-session--touch) #'ignore)
                  ((symbol-function 'pi-rpc-send)
                   (lambda (_rpc command callback)
                     (pcase (cdr (assoc "type" command))
                       ("get_state"
                        (funcall callback
                                 `(:success t :data (:sessionFile ,actual
                                                :sessionId "actual"))))
                       ("switch_session"
                        (funcall callback '(:success t :data (:cancelled t))))))))
          (pi-session--bootstrap-after-spawn session))
      (delete-file pi-session-active-index-file))
    (should (eq (pi-session-status session) 'ready))
    (should (equal (pi-session-session-file session) actual))
    (should-not (seq-some (lambda (event)
                            (equal (plist-get event :type) "session_error"))
                          events))))

(defconst pi-test--tree-entries
  '((:type "message" :id "user-root" :parentId nil :timestamp "1"
     :message (:role "user" :content ((:type "text" :text "root"))))
    (:type "message" :id "assistant-root" :parentId "user-root" :timestamp "2"
     :message (:role "assistant" :content ((:type "text" :text "answer"))))
    (:type "message" :id "short-leaf" :parentId "assistant-root" :timestamp "3"
     :message (:role "user" :content ((:type "text" :text "short"))))
    (:type "label" :id "label-entry" :parentId "short-leaf" :timestamp "4"
     :targetId "short-leaf" :label "chosen")
    (:type "message" :id "long-branch" :parentId "assistant-root" :timestamp "5"
     :message (:role "user" :content ((:type "text" :text "long"))))
    (:type "message" :id "long-leaf" :parentId "long-branch" :timestamp "6"
     :message (:role "assistant" :content ((:type "text" :text "later"))))
    (:type "custom" :id "hidden-mark" :parentId "long-leaf" :timestamp "7"
     :customType "pi.el/tree"))
  "Synthetic branching session entries for tree tests.")

(ert-deftest pi-session-tree-builder-marks-leaf-active ()
  (let* ((nodes (pi-session--build-tree-nodes pi-test--tree-entries "short-leaf"))
         (current (seq-find (lambda (node) (plist-get node :current)) nodes)))
    (should (equal (plist-get current :id) "short-leaf"))))

(ert-deftest pi-session-tree-builder-hides-custom-and-label-entries ()
  (let ((nodes (pi-session--build-tree-nodes pi-test--tree-entries "short-leaf")))
    (should-not (seq-find (lambda (node)
                           (member (plist-get node :id)
                                   '("label-entry" "hidden-mark")))
                         nodes))
    (should (string-match-p
             "\\[chosen\\]"
             (plist-get (seq-find (lambda (node)
                                    (equal (plist-get node :id) "short-leaf"))
                                  nodes)
                        :display)))))

(ert-deftest pi-session-tree-builder-falls-back-without-leaf ()
  (let* ((nodes (pi-session--build-tree-nodes pi-test--tree-entries nil))
         (current (seq-find (lambda (node) (plist-get node :current)) nodes)))
    (should (equal (plist-get current :id) "long-leaf"))))

(ert-deftest pi-session-get-entries-adds-optional-cursor ()
  (let ((session (pi-session--create
                  :id "test-session" :status 'ready :rpc 'fake-rpc))
        sent)
    (cl-letf (((symbol-function 'pi-rpc-live-p) (lambda (_rpc) t))
              ((symbol-function 'pi-session--touch) #'ignore)
              ((symbol-function 'pi-rpc-send)
               (lambda (_rpc command callback)
                 (setq sent command)
                 (funcall callback '(:success t :data (:entries nil :leafId nil))))))
      (pi-session-get-entries session #'ignore "cursor-1"))
    (should (equal sent '(("type" . "get_entries") ("since" . "cursor-1"))))))

(ert-deftest pi-session-send-prompt-refreshes-state-before-prompt ()
  (let* ((sent-commands nil)
         (session (pi-session--create
                   :id "test-session"
                   :status 'ready
                   :cached-state '(:is-streaming t)
                   :rpc 'fake-rpc)))
    (cl-letf (((symbol-function 'pi-rpc-live-p) (lambda (_rpc) t))
              ((symbol-function 'pi-session--touch) #'ignore)
              ((symbol-function 'pi-rpc-send)
               (lambda (_rpc command callback)
                 (push command sent-commands)
                 (if (equal (cdr (assoc "type" command)) "get_state")
                     (funcall callback '(:success t :data (:isStreaming :json-false)))
                   (funcall callback '(:success t)))
                 "req-test")))
      (pi-session-send-prompt session "hello"))
    (let ((prompt-command (car sent-commands)))
      (should (equal (cdr (assoc "type" prompt-command)) "prompt"))
      (should-not (assoc "streamingBehavior" prompt-command)))))

(ert-deftest pi-session-send-prompt-streaming-uses-steer-behavior ()
  (let* ((sent-commands nil)
         (session (pi-session--create
                   :id "test-session"
                   :status 'ready
                   :cached-state '(:is-streaming :json-false)
                   :rpc 'fake-rpc)))
    (cl-letf (((symbol-function 'pi-rpc-live-p) (lambda (_rpc) t))
              ((symbol-function 'pi-session--touch) #'ignore)
              ((symbol-function 'pi-rpc-send)
               (lambda (_rpc command callback)
                 (push command sent-commands)
                 (if (equal (cdr (assoc "type" command)) "get_state")
                     (funcall callback '(:success t :data (:isStreaming t)))
                   (funcall callback '(:success t)))
                 "req-test")))
      (pi-session-send-prompt session "hello"))
    (let ((prompt-command (car sent-commands)))
      (should (equal (cdr (assoc "type" prompt-command)) "prompt"))
      (should (equal (cdr (assoc "streamingBehavior" prompt-command)) "steer")))))

(ert-deftest pi-session-dir-resolution-current-defaults ()
  (let ((pi-session-directory nil)
        (pi-rpc-extra-args nil)
        (process-environment (cons "PI_CODING_AGENT_SESSION_DIR=/tmp/pi-env-sessions"
                                   process-environment)))
    (should (equal (pi-session--base-session-directory)
                   (file-name-as-directory "/tmp/pi-env-sessions"))))
  (let ((pi-session-directory nil)
        (pi-rpc-extra-args '("--foo" "bar" "--session-dir" "/tmp/pi-arg-sessions")))
    (should (equal (pi-session--base-session-directory)
                   (file-name-as-directory "/tmp/pi-arg-sessions"))))
  (let ((pi-session-directory nil)
        (pi-rpc-extra-args '("--session-dir=/tmp/pi-equals-sessions")))
    (should (equal (pi-session--base-session-directory)
                   (file-name-as-directory "/tmp/pi-equals-sessions"))))
  (let ((pi-session-directory "/tmp/pi-custom-sessions")
        (pi-rpc-extra-args '("--session-dir" "/tmp/pi-arg-sessions")))
    (should (equal (pi-session--base-session-directory)
                   (file-name-as-directory "/tmp/pi-custom-sessions")))))

(ert-deftest pi-current-command-json-shapes ()
  (should (equal (pi-rpc--encode-command
                  '(("type" . "set_auto_retry") ("enabled" . :json-false)))
                 "{\"type\":\"set_auto_retry\",\"enabled\":false}\n"))
  (should (equal (pi-rpc--encode-command
                  '(("type" . "set_follow_up_mode") ("mode" . "one-at-a-time")))
                 "{\"type\":\"set_follow_up_mode\",\"mode\":\"one-at-a-time\"}\n")))

(provide 'pi-current-test)

;;; pi-current-test.el ends here
