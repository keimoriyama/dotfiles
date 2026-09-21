;;; claude-usage-tests.el --- Tests for claude-usage -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))

(load (expand-file-name "claude-usage.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest claude-usage-parse-keeps-spent-percentages ()
  (let ((state (claude-usage--parse
                "{\"five_hour\":{\"utilization_pct\":32,\"resets_at\":1788499800},\
\"seven_day\":{\"utilization_pct\":47,\"resets_at\":1788769200}}")))
    (should (equal 32 (plist-get state :five-hour)))
    (should (equal 47 (plist-get state :seven-day)))
    (should (equal 1788499800 (plist-get state :five-hour-reset)))
    (should (equal 1788769200 (plist-get state :seven-day-reset)))
    (should (null (plist-get state :error)))
    (should (plist-get state :updated-at))))

(defun claude-usage-tests--account-file (contents)
  "Write CONTENTS to a temporary stand-in for Claude Code's account file."
  (let ((file (make-temp-file "claude-usage-account" nil ".json")))
    (with-temp-file file (insert contents))
    file))

(ert-deftest claude-usage-enterprise-p-reads-the-seat-from-the-account-file ()
  (let ((claude-usage--enterprise-cache nil))
    (let ((claude-usage-account-file
           (claude-usage-tests--account-file
            "{\"oauthAccount\":{\"organizationType\":\"claude_enterprise\",\
\"seatTier\":\"enterprise_usage_based\"}}")))
      (should (claude-usage--enterprise-p)))
    (setq claude-usage--enterprise-cache nil)
    ;; Either field alone is enough.
    (let ((claude-usage-account-file
           (claude-usage-tests--account-file
            "{\"oauthAccount\":{\"organizationType\":\"renamed\",\
\"seatTier\":\"enterprise_usage_based\"}}")))
      (should (claude-usage--enterprise-p)))
    (setq claude-usage--enterprise-cache nil)
    (let ((claude-usage-account-file
           (claude-usage-tests--account-file
            "{\"oauthAccount\":{\"organizationType\":\"claude_max\",\
\"seatTier\":\"max\"}}")))
      (should-not (claude-usage--enterprise-p)))))

(ert-deftest claude-usage-enterprise-p-tolerates-a-missing-or-broken-file ()
  (let ((claude-usage--enterprise-cache nil)
        (claude-usage-account-file "/nonexistent/claude.json"))
    (should-not (claude-usage--enterprise-p)))
  (let* ((claude-usage--enterprise-cache nil)
         (claude-usage-account-file
          (claude-usage-tests--account-file "{not json")))
    (should-not (claude-usage--enterprise-p))))

(ert-deftest claude-usage-enterprise-p-caches-until-the-file-changes ()
  "The account file carries every cached feature flag; one parse per change."
  (let* ((claude-usage--enterprise-cache nil)
         (reads 0)
         (claude-usage-account-file
          (claude-usage-tests--account-file
           "{\"oauthAccount\":{\"seatTier\":\"enterprise_usage_based\"}}")))
    (cl-letf* ((original (symbol-function 'claude-usage--read-enterprise))
               ((symbol-function 'claude-usage--read-enterprise)
                (lambda (file) (setq reads (1+ reads)) (funcall original file))))
      (should (claude-usage--enterprise-p))
      (should (claude-usage--enterprise-p))
      (should (equal 1 reads)))))

(ert-deftest claude-usage-parse-keeps-the-org-spending-limit ()
  (let ((state (claude-usage--parse
                "{\"five_hour\":{\"utilization_pct\":0,\"resets_at\":null},\
\"seven_day\":{\"utilization_pct\":0,\"resets_at\":null},\
\"org\":{\"utilization_pct\":61,\"used_usd\":131.95,\"limit_usd\":215,\
\"currency\":\"USD\",\"resets_at\":1790812800}}")))
    (should (equal 61 (plist-get state :org)))
    (should (equal 131.95 (plist-get state :org-used-usd)))
    (should (equal 215 (plist-get state :org-limit-usd)))
    (should (equal "USD" (plist-get state :org-currency)))
    (should (equal 1790812800 (plist-get state :org-reset)))))

(ert-deftest claude-usage-parse-tolerates-a-contract-without-an-org-limit ()
  "A contract with no spending limit reports org as null."
  (let ((state (claude-usage--parse
                "{\"five_hour\":{\"utilization_pct\":32},\
\"seven_day\":{\"utilization_pct\":47},\"org\":null}")))
    (should (null (plist-get state :org)))
    (should (null (plist-get state :org-used-usd)))
    (should (equal 32 (plist-get state :five-hour)))))

(ert-deftest claude-usage-parse-rejects-missing-windows ()
  (should-error (claude-usage--parse
                 "{\"seven_day\":{\"utilization_pct\":47}}"))
  (should-error (claude-usage--parse
                 "{\"five_hour\":{\"utilization_pct\":32}}")))

(ert-deftest claude-usage-parse-rejects-non-numeric-percentages ()
  (should-error (claude-usage--parse
                 "{\"five_hour\":{\"utilization_pct\":\"32\"},\
\"seven_day\":{\"utilization_pct\":47}}")))

(ert-deftest claude-usage-parse-rejects-malformed-json ()
  (should-error (claude-usage--parse "not json")))

(ert-deftest claude-usage-format-mode-line-matches-the-shared-layout ()
  "Claude reads like Codex: spent percentage then reset countdown."
  (should (equal " Claude [5h 32%↻1h0m · 7d 47%↻2d3h]"
                 (substring-no-properties
                  (claude-usage--format-mode-line
                   '(:five-hour 32 :seven-day 47
                     :five-hour-reset 4600 :seven-day-reset 187000
                     :error nil)
                   1000)))))

(ert-deftest claude-usage-format-mode-line-prefers-the-org-spending-limit ()
  "On a contract billed against a limit, the spend replaces the windows."
  (should (equal " Claude [Org 61% $132/$215↻2d3h]"
                 (substring-no-properties
                  (claude-usage--format-mode-line
                   '(:five-hour 0 :seven-day 0
                     :five-hour-reset :null :seven-day-reset :null
                     :org 61 :org-used-usd 131.95 :org-limit-usd 215
                     :org-currency "USD" :org-reset 187000
                     :error nil)
                   1000)))))

(ert-deftest claude-usage-format-mode-line-omits-an-unknown-spend ()
  "Without the dollar figures the org share still stands on its own."
  (should (equal " Claude [Org 61%↻-]"
                 (substring-no-properties
                  (claude-usage--format-mode-line
                   '(:five-hour 0 :seven-day 0
                     :org 61 :org-used-usd nil :org-limit-usd nil
                     :org-currency nil :org-reset :null
                     :error nil)
                   1000)))))

(ert-deftest claude-usage-format-mode-line-keeps-the-org-window-on-a-failed-fetch ()
  "An Enterprise seat reports an unknown spend, not an untouched quota."
  (should (equal " Claude [Org --%↻-]*"
                 (substring-no-properties
                  (claude-usage--format-mode-line
                   '(:five-hour 0 :seven-day 0
                     :five-hour-reset :null :seven-day-reset :null
                     :org nil :enterprise t
                     :error "boom")
                   1000)))))

(ert-deftest claude-usage-format-mode-line-keeps-the-windows-off-an-enterprise-seat ()
  (should (equal " Claude [5h 32%↻1h0m · 7d 47%↻2d3h]"
                 (substring-no-properties
                  (claude-usage--format-mode-line
                   '(:five-hour 32 :seven-day 47
                     :five-hour-reset 4600 :seven-day-reset 187000
                     :org nil :enterprise nil
                     :error nil)
                   1000)))))

(ert-deftest claude-usage-format-mode-line-survives-a-null-reset ()
  "The CLI reports resets_at as null when it has no window data."
  (should (equal " Claude [5h 0%↻- · 7d 0%↻-]"
                 (substring-no-properties
                  (claude-usage--format-mode-line
                   '(:five-hour 0 :seven-day 0
                     :five-hour-reset :null :seven-day-reset :null
                     :error nil)
                   1000)))))

(ert-deftest claude-usage-format-mode-line-marks-stale-values ()
  (should (equal " Claude [5h 32%↻1h0m · 7d 47%↻-]*"
                 (substring-no-properties
                  (claude-usage--format-mode-line
                   '(:five-hour 32 :seven-day 47
                     :five-hour-reset 4600 :seven-day-reset nil
                     :error "boom")
                   1000)))))

(ert-deftest claude-usage-format-mode-line-falls-back-before-first-success ()
  (should (equal " Claude ?"
                 (claude-usage--format-mode-line
                  '(:five-hour nil :seven-day nil :error "boom")))))

(ert-deftest claude-usage-mode-line-follows-the-current-agent ()
  (with-temp-buffer
    (should-not (claude-usage--mode-line))
    (setq major-mode 'agent-shell-mode)
    (cl-letf (((symbol-function 'agent-shell-get-config)
               (lambda (_) '((:identifier . codex)))))
      (should-not (claude-usage--mode-line)))
    (cl-letf (((symbol-function 'agent-shell-get-config)
               (lambda (_) '((:identifier . claude-code)))))
      (should (eq 'claude-usage-mode-line-string
                  (claude-usage--mode-line))))
    (cl-letf (((symbol-function 'agent-shell-get-config)
               (lambda (_) '((:identifier . unsupported)))))
      (should-not (claude-usage--mode-line)))))

(ert-deftest claude-usage-fetch-sends-eof-so-the-cli-does-not-block-on-stdin ()
  "The CLI waits for stdin JSON from Claude Code's status-line caller;
outside that context it must see an immediate EOF or it hangs."
  (let ((claude-usage--process nil)
        (eof-sent-to nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _args) 'fake-process))
              ((symbol-function 'process-send-eof)
               (lambda (proc) (setq eof-sent-to proc))))
      (claude-usage--fetch)
      (should (eq eof-sent-to 'fake-process)))))

(ert-deftest claude-usage-fetch-records-an-error-when-the-cli-is-missing ()
  (let ((claude-usage--process nil)
        (claude-usage--state '(:five-hour nil :seven-day nil :error nil))
        (claude-usage-mode-line-string ""))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _args) (error "no such file"))))
      (claude-usage--fetch)
      (should (plist-get claude-usage--state :error)))))

(ert-deftest claude-usage-mode-registers-the-mode-line-string-once ()
  ;; The mode is global, so put the session back the way it was found.
  (let ((original (default-value 'mode-line-format))
        (was-enabled claude-usage-mode)
        (string claude-usage-mode-line-string))
    (unwind-protect
        (cl-letf (((symbol-function 'claude-usage--start-timer) #'ignore)
                  ((symbol-function 'claude-usage--stop-timer) #'ignore))
          (setq-default mode-line-format
                        '("" mode-line-buffer-identification "  "
                          mode-line-misc-info))
          (claude-usage-mode 1)
          (claude-usage-mode 1)
          (should (equal '("" mode-line-buffer-identification
                           (:eval (claude-usage--mode-line)) "  "
                           mode-line-misc-info)
                         (default-value 'mode-line-format)))
          (claude-usage-mode -1)
          (should (equal '("" mode-line-buffer-identification "  "
                           mode-line-misc-info)
                         (default-value 'mode-line-format))))
      (setq claude-usage-mode was-enabled
            claude-usage-mode-line-string string)
      (setq-default mode-line-format original))))

(ert-deftest claude-usage-mode-line-sits-next-to-the-buffer-name ()
  "The segment goes right after the buffer name, not at the far right,
where a narrow window would cut it off."
  (let ((original (default-value 'mode-line-format)))
    (unwind-protect
        (progn
          (setq-default mode-line-format
                        '("" mode-line-buffer-identification
                          mode-line-modes mode-line-misc-info))
          (claude-usage--install-mode-line)
          (should (< (seq-position (default-value 'mode-line-format)
                                   claude-usage--mode-line-spec)
                     (seq-position (default-value 'mode-line-format)
                                   'mode-line-modes))))
      (setq-default mode-line-format original))))

(ert-deftest claude-usage-record-error-keeps-the-last-good-values ()
  (let ((claude-usage--state '(:five-hour 32 :seven-day 47 :error nil))
        (claude-usage-mode-line-string ""))
    (claude-usage--record-error "boom")
    (should (equal 32 (plist-get claude-usage--state :five-hour)))
    (should (equal "boom" (plist-get claude-usage--state :error)))
    (should (string-match-p "\\` Claude \\[5h 32%↻.* · 7d 47%↻.*\\]\\*\\'"
                            claude-usage-mode-line-string))))

(provide 'claude-usage-tests)

;;; claude-usage-tests.el ends here
