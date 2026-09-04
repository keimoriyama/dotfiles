;;; agent-shell-provider-usage-tests.el --- Tests for provider usage commands -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(load (expand-file-name "agent-shell-provider-usage.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest agent-shell-provider-usage-uses-claude-usage-command ()
  "A Claude Code session requests its account usage."
  (let (submitted)
    (cl-letf (((symbol-function 'agent-shell-get-config)
               (lambda (_) '((:identifier . claude-code))))
              ((symbol-function 'agent-shell-prompt-queue)
               (lambda (prompt) (setq submitted prompt))))
      (my-agent-shell-show-provider-usage)
      (should (equal submitted "/usage")))))

(ert-deftest agent-shell-provider-usage-uses-codex-status-command ()
  "A Codex session requests usage windows and reset times."
  (let (submitted)
    (cl-letf (((symbol-function 'agent-shell-get-config)
               (lambda (_) '((:identifier . codex))))
              ((symbol-function 'agent-shell-prompt-queue)
               (lambda (prompt) (setq submitted prompt))))
      (my-agent-shell-show-provider-usage)
      (should (equal submitted "/status")))))

(ert-deftest agent-shell-provider-usage-rejects-an-unsupported-agent ()
  "An unsupported agent does not receive a guessed usage command."
  (cl-letf (((symbol-function 'agent-shell-get-config)
             (lambda (_) '((:identifier . opencode)))))
    (should-error (my-agent-shell-show-provider-usage) :type 'user-error)))

(ert-deftest agent-shell-provider-usage-reads-codex-rollout-rate-limits ()
  "The newest valid Codex event supplies both normal account windows."
  (let ((directory (make-temp-file "codex-sessions-" t)))
    (unwind-protect
        (let ((my-agent-shell-codex-sessions-directory directory))
          (with-temp-file (expand-file-name "rollout.jsonl" directory)
            (insert "{not-json}\n")
            (insert "{\"payload\":{\"rate_limits\":{"
                    "\"primary\":{\"used_percent\":63.4,"
                    "\"window_minutes\":300,\"resets_at\":4600},"
                    "\"secondary\":{\"used_percent\":30,"
                    "\"window_minutes\":10080,\"resets_at\":87400}}}}\n"))
          (should
           (equal (my-agent-shell--read-codex-rate-limits)
                  '((:label "5h" :used 63 :reset 4600)
                    (:label "7d" :used 30 :reset 87400)))))
      (delete-directory directory t))))

(ert-deftest agent-shell-provider-usage-handles-missing-codex-rollouts ()
  "A missing Codex sessions directory produces no mode-line data."
  (let ((my-agent-shell-codex-sessions-directory
         (make-temp-name temporary-file-directory)))
    (should-not (my-agent-shell--read-codex-rate-limits))))

(ert-deftest agent-shell-provider-usage-formats-percent-and-reset-time ()
  "Rate-limit windows show usage, reset countdowns, and missing values."
  (should
   (equal
    (substring-no-properties
     (my-agent-shell--format-rate-limits
      '((:label "5h" :used 95 :reset 4600)
        (:label "7d" :used nil :reset nil))
      1000))
    " [5h 95%↻1h · 7d --%↻-]"))
  (should
   (equal
    (substring-no-properties
     (my-agent-shell--format-rate-limits
      '((:label "5h" :used 10 :reset 999)) 1000))
    " [5h 10%↻now]"))
  (should-not (my-agent-shell--format-rate-limits nil 1000)))

(ert-deftest agent-shell-provider-usage-renders-provider-mode-line ()
  "The mode-line renderer uses Codex data and hides providers it cannot read."
  (with-temp-buffer
    (let ((my-agent-shell--codex-rate-limit-cache
           (list :checked-at (float-time)
                 :limits '((:label "5h" :used 25 :reset nil)))))
      (cl-letf (((symbol-function 'agent-shell-get-config)
                 (lambda (_) '((:identifier . codex)))))
        ;; %% is what the mode line renders as a single literal percent sign.
        (should (string-match-p "5h 25%%↻"
                                (my-agent-shell-rate-limit-mode-line))))
      ;; Claude's windows come from `claude-usage', not from here.
      (cl-letf (((symbol-function 'agent-shell-get-config)
                 (lambda (_) '((:identifier . claude-code)))))
        (should-not (my-agent-shell-rate-limit-mode-line))))))

(ert-deftest agent-shell-provider-usage-setup-is-buffer-local-and-idempotent ()
  "Setup adds one buffer-local segment next to the buffer name, even when called twice."
  (with-temp-buffer
    (let ((global-value (default-value 'mode-line-format)))
      (my-agent-shell-provider-usage-setup)
      (my-agent-shell-provider-usage-setup)
      (should (local-variable-p 'mode-line-format))
      (should (= (cl-count my-agent-shell--rate-limit-mode-line-spec
                            mode-line-format :test #'equal)
                 1))
      (should (equal global-value (default-value 'mode-line-format))))))

(provide 'agent-shell-provider-usage-tests)
;;; agent-shell-provider-usage-tests.el ends here
