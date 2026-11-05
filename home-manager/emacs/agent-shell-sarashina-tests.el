;;; agent-shell-sarashina-tests.el --- Tests for the Sarashina chat agent -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))

(load (expand-file-name "agent-shell-sarashina.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest agent-shell-sarashina-environment-points-opencode-at-the-chat-config ()
  "OpenCode reads the dedicated config and the key, ignoring project setup."
  (let ((my-agent-shell-sarashina-opencode-config "/tmp/sarashina.json"))
    (let ((environment (my-agent-shell-sarashina-environment "secret")))
      (should (member "OPENCODE_CONFIG=/tmp/sarashina.json" environment))
      (should (member "SARASHINA_API_KEY=secret" environment))
      (should (member "OPENCODE_DISABLE_PROJECT_CONFIG=1" environment))
      (should (member "OPENCODE_DISABLE_CLAUDE_CODE=1" environment)))))

(ert-deftest agent-shell-sarashina-client-runs-opencode-acp-without-opencode-auth ()
  "The client starts `opencode acp' with the Sarashina environment."
  (let ((agent-shell-opencode-authentication nil)
        (agent-shell-opencode-environment '("EXTRA=1")))
    (cl-letf (((symbol-function 'my-agent-shell-sarashina--api-key)
               (lambda () "secret")))
      (with-temp-buffer
        (let* ((client (my-agent-shell-sarashina-make-client (current-buffer)))
               (environment (map-elt client :environment-variables)))
          (should (equal (map-elt client :command) "opencode"))
          (should (equal (map-elt client :command-params) '("acp")))
          (should (member "SARASHINA_API_KEY=secret" environment))
          (should (member "EXTRA=1" environment))
          (should-not (cl-find-if (lambda (entry)
                                    (string-prefix-p "OPENCODE_API_KEY=" entry))
                                  environment)))))))

(ert-deftest agent-shell-sarashina-api-key-comes-from-the-environment ()
  "The key is read from SARASHINA_API_KEY and a missing one is reported."
  (let ((process-environment (cons "SARASHINA_API_KEY=secret" process-environment)))
    (should (equal (my-agent-shell-sarashina--api-key) "secret")))
  (let ((process-environment (cons "SARASHINA_API_KEY" process-environment)))
    (should-error (my-agent-shell-sarashina--api-key) :type 'user-error))
  (let ((process-environment (cons "SARASHINA_API_KEY=" process-environment)))
    (should-error (my-agent-shell-sarashina--api-key) :type 'user-error)))

(ert-deftest agent-shell-sarashina-config-starts-in-chat-mode ()
  "A new Sarashina shell starts in the tool-less chat agent."
  (let ((config (my-agent-shell-sarashina-make-agent-config)))
    (should (eq (map-elt config :identifier) 'sarashina))
    (should (equal (funcall (map-elt config :default-session-mode-id)) "chat"))
    (should (equal (funcall (map-elt config :default-model-id))
                   "sarashina/sarashina3.2-mini-thinking"))))

(provide 'agent-shell-sarashina-tests)
;;; agent-shell-sarashina-tests.el ends here
