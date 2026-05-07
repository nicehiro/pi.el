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

(ert-deftest pi-thinking-level-map-filters-supported-levels ()
  (let ((model '(:thinkingLevelMap (:off t :minimal :json-false :low t :medium t :high :json-false))))
    (should (equal (pi--supported-thinking-levels model)
                   '("off" "low" "medium")))))

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
              :autoRetryEnabled :json-false
              :isRetrying :json-false
              :sessionFile "/tmp/pi-test.jsonl"
              :sessionId "sid")))
    (should-not (plist-get (pi-session-cached-state session) :is-streaming))
    (should-not (plist-get (pi-session-cached-state session) :is-compacting))
    (should-not (plist-get (pi-session-cached-state session) :auto-compaction-enabled))
    (should-not (plist-get (pi-session-cached-state session) :auto-retry-enabled))
    (should-not (plist-get (pi-session-cached-state session) :is-retrying))))

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

(ert-deftest pi-session-send-prompt-json-false-streaming-uses-prompt ()
  (let* ((sent-command nil)
         (session (pi-session--create
                   :id "test-session"
                   :status 'ready
                   :cached-state '(:is-streaming :json-false)
                   :rpc 'fake-rpc)))
    (cl-letf (((symbol-function 'pi-rpc-live-p) (lambda (_rpc) t))
              ((symbol-function 'pi-session--touch) #'ignore)
              ((symbol-function 'pi-rpc-send)
               (lambda (_rpc command callback)
                 (setq sent-command command)
                 (funcall callback '(:success t))
                 "req-test")))
      (pi-session-send-prompt session "hello"))
    (should (equal (cdr (assoc "type" sent-command)) "prompt"))))

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
