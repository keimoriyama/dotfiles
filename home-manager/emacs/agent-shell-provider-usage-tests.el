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

(ert-deftest agent-shell-provider-usage-captures-claude-rate-limit-events ()
  "Claude rate-limit events update one known window at a time and ignore unknown ones."
  (with-temp-buffer
    (let ((state `((:buffer . ,(current-buffer))))
          (five-hour '((_meta
                        (_claude/rateLimit
                         (status . "allowed")
                         (rateLimitType . "five_hour")
                         (utilization . 0.72)
                         (resetsAt . 4600)))))
          (seven-day '((_meta
                        (_claude/rateLimit
                         (status . "allowed")
                         (rateLimitType . "seven_day")
                         (utilization . 0.01)
                         (resetsAt . 87400)))))
          (unknown '((_meta
                      (_claude/rateLimit
                       (status . "allowed")
                       (rateLimitType . "seven_day_overage_included")
                       (utilization . 0.5)
                       (resetsAt . 4600))))))
      (my-agent-shell--capture-claude-rate-limit
       :state state :acp-update five-hour)
      (my-agent-shell--capture-claude-rate-limit
       :state state :acp-update seven-day)
      (should (equal (alist-get "5h" my-agent-shell--claude-rate-limits
                                nil nil #'equal)
                     '(:label "5h" :used 72 :reset 4600 :status "allowed")))
      (should (equal (alist-get "7d" my-agent-shell--claude-rate-limits
                                nil nil #'equal)
                     '(:label "7d" :used 1 :reset 87400 :status "allowed")))
      (my-agent-shell--capture-claude-rate-limit
       :state state :acp-update unknown)
      (should (= (length my-agent-shell--claude-rate-limits) 2)))))

(ert-deftest agent-shell-provider-usage-captures-raw-claude-rate-limit-events ()
  "A raw SDK rate-limit notification supplies the utilization session updates omit."
  (with-temp-buffer
    (let ((state `((:buffer . ,(current-buffer))))
          (raw '((method . "_claude/sdkMessage")
                 (params
                  (sessionId . "abc")
                  (message
                   (type . "rate_limit_event")
                   (rate_limit_info
                    (status . "allowed_warning")
                    (rateLimitType . "five_hour")
                    (utilization . 0.81)
                    (resetsAt . 4600))))))
          (other '((method . "session/update")
                   (params (update (sessionUpdate . "agent_message_chunk"))))))
      (my-agent-shell--capture-claude-raw-rate-limit
       :state state :acp-notification raw)
      (should (equal (alist-get "5h" my-agent-shell--claude-rate-limits
                                nil nil #'equal)
                     '(:label "5h" :used 81 :reset 4600
                              :status "allowed_warning")))
      (my-agent-shell--capture-claude-raw-rate-limit
       :state state :acp-notification other)
      (should (= (length my-agent-shell--claude-rate-limits) 1)))))

(ert-deftest agent-shell-provider-usage-shows-status-when-utilization-is-absent ()
  "A window Claude Code reports without a percentage still shows its status.
This is the event a usage-based seat actually receives."
  (with-temp-buffer
    (let ((state `((:buffer . ,(current-buffer))))
          (raw '((method . "_claude/sdkMessage")
                 (params
                  (message
                   (type . "rate_limit_event")
                   (rate_limit_info
                    (status . "allowed")
                    (resetsAt . 4600)
                    (rateLimitType . "overage")
                    (overageStatus . "allowed")
                    (overageInUse . t)))))))
      (my-agent-shell--capture-claude-raw-rate-limit
       :state state :acp-notification raw)
      (should (equal (alist-get "overage" my-agent-shell--claude-rate-limits
                                nil nil #'equal)
                     '(:label "overage" :used nil :reset 4600
                              :status "allowed")))
      (should (equal (substring-no-properties
                      (my-agent-shell--format-rate-limits
                       (list (alist-get "overage" my-agent-shell--claude-rate-limits
                                        nil nil #'equal))
                       1000))
                     " [overage ok↻1h]")))))

(ert-deftest agent-shell-provider-usage-requests-raw-rate-limit-events ()
  "The session asks for rate-limit events without dropping its other metadata."
  (let* ((config
          (list (cons :identifier 'claude-code)
                (cons :session-meta
                      (list (cons 'claudeCode
                                  (list (cons 'options
                                              (list (cons 'thinking
                                                          (list (cons 'display "summarized")))))))))))
         (original (copy-tree config))
         (updated (my-agent-shell-request-claude-rate-limit-events config)))
    (should (equal (map-nested-elt updated '(:session-meta claudeCode emitRawSDKMessages))
                   my-agent-shell--claude-raw-sdk-message-filter))
    (should (equal (map-nested-elt updated '(:session-meta claudeCode options thinking display))
                   "summarized"))
    (should (equal (map-elt updated :identifier) 'claude-code))
    ;; The package hands out a quoted literal, so it must come back untouched.
    (should (equal config original))))

(ert-deftest agent-shell-provider-usage-reads-claude-status-line-file ()
  "The status-line file supplies a percentage for every window it names."
  (let ((file (make-temp-file "claude-rate-limits-" nil ".json")))
    (unwind-protect
        (let ((my-agent-shell-claude-rate-limit-file file)
              (my-agent-shell-claude-rate-limit-staleness 900))
          (with-temp-file file
            (insert "{\"five_hour\":{\"used_percentage\":34.7,\"resets_at\":4600},"
                    "\"seven_day\":{\"used_percentage\":12,\"resets_at\":87400},"
                    "\"made_up_window\":{\"used_percentage\":99,\"resets_at\":1}}"))
          (should (equal (my-agent-shell--read-claude-rate-limit-file)
                         '((:label "5h" :used 35 :reset 4600 :stale nil)
                           (:label "7d" :used 12 :reset 87400 :stale nil)))))
      (delete-file file))))

(ert-deftest agent-shell-provider-usage-marks-a-stale-status-line-file ()
  "Values older than the staleness limit are flagged rather than shown as current."
  (let ((file (make-temp-file "claude-rate-limits-" nil ".json")))
    (unwind-protect
        (let ((my-agent-shell-claude-rate-limit-file file)
              (my-agent-shell-claude-rate-limit-staleness 0))
          (with-temp-file file
            (insert "{\"five_hour\":{\"used_percentage\":34,\"resets_at\":4600}}"))
          (should (plist-get (car (my-agent-shell--read-claude-rate-limit-file))
                             :stale))
          (should (equal (substring-no-properties
                          (my-agent-shell--format-rate-limits
                           (my-agent-shell--read-claude-rate-limit-file) 1000))
                         " [5h 34%↻1h]")))
      (delete-file file))))

(ert-deftest agent-shell-provider-usage-prefers-live-events-over-the-file ()
  "A window captured over ACP wins; the rest still come from the file."
  (let ((file (make-temp-file "claude-rate-limits-" nil ".json")))
    (unwind-protect
        (with-temp-buffer
          (let ((my-agent-shell-claude-rate-limit-file file)
                (my-agent-shell-claude-rate-limit-staleness 900)
                (my-agent-shell--claude-rate-limit-file-cache nil))
            (with-temp-file file
              (insert "{\"five_hour\":{\"used_percentage\":34,\"resets_at\":4600},"
                      "\"seven_day\":{\"used_percentage\":12,\"resets_at\":87400}}"))
            (setq my-agent-shell--claude-rate-limits
                  '(("5h" :label "5h" :used 88 :reset 4600
                     :status "allowed_warning")))
            (should (equal (my-agent-shell--claude-mode-line-windows)
                           '((:label "5h" :used 88 :reset 4600
                                     :status "allowed_warning")
                             (:label "7d" :used 12 :reset 87400 :stale nil))))))
      (delete-file file))))

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
  "The mode-line renderer uses session data and hides unsupported providers."
  (with-temp-buffer
    (setq my-agent-shell--claude-rate-limits
          '(("5h" :label "5h" :used 25 :reset nil)))
    (cl-letf (((symbol-function 'agent-shell-get-config)
               (lambda (_) '((:identifier . claude-code)))))
      ;; %% is what the mode line renders as a single literal percent sign.
      (should (string-match-p "5h 25%%↻"
                              (my-agent-shell-rate-limit-mode-line))))
    (cl-letf (((symbol-function 'agent-shell-get-config)
               (lambda (_) '((:identifier . opencode)))))
      (should-not (my-agent-shell-rate-limit-mode-line)))))

(ert-deftest agent-shell-provider-usage-orders-claude-windows-shortest-first ()
  "Windows render shortest-first regardless of the order they were captured in."
  (with-temp-buffer
    (setq my-agent-shell--claude-rate-limits
          '(("7d" :label "7d" :used 2 :reset nil)
            ("5h" :label "5h" :used 27 :reset nil)))
    (cl-letf (((symbol-function 'agent-shell-get-config)
               (lambda (_) '((:identifier . claude-code)))))
      (should (equal (substring-no-properties
                      (my-agent-shell-rate-limit-mode-line))
                     " [5h 27%%↻- · 7d 2%%↻-]")))))

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
