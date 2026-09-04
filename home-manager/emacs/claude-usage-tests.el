;;; claude-usage-tests.el --- Tests for claude-usage -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "claude-usage.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest claude-usage-parse-computes-remaining-percentages ()
  (let ((state (claude-usage--parse
                "{\"five_hour\":{\"utilization_pct\":32,\"resets_at\":1788499800},\
\"seven_day\":{\"utilization_pct\":47,\"resets_at\":1788769200}}")))
    (should (equal 68 (plist-get state :five-hour)))
    (should (equal 53 (plist-get state :seven-day)))
    (should (equal 1788499800 (plist-get state :five-hour-reset)))
    (should (equal 1788769200 (plist-get state :seven-day-reset)))
    (should (null (plist-get state :error)))
    (should (plist-get state :updated-at))))

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

(ert-deftest claude-usage-format-mode-line-shows-remaining-percentages ()
  (should (equal " Claude 5h 68%% | 7d 53%%"
                 (claude-usage--format-mode-line
                  '(:five-hour 68 :seven-day 53 :error nil)))))

(ert-deftest claude-usage-format-mode-line-marks-stale-values ()
  (should (equal " Claude 5h 68%% | 7d 53%%*"
                 (claude-usage--format-mode-line
                  '(:five-hour 68 :seven-day 53 :error "boom")))))

(ert-deftest claude-usage-format-mode-line-falls-back-before-first-success ()
  (should (equal " Claude ?"
                 (claude-usage--format-mode-line
                  '(:five-hour nil :seven-day nil :error "boom")))))

(ert-deftest claude-usage-mode-registers-the-mode-line-string-once ()
  (let ((global-mode-string nil))
    (cl-letf (((symbol-function 'claude-usage--start-timer) #'ignore)
              ((symbol-function 'claude-usage--stop-timer) #'ignore))
      (claude-usage-mode 1)
      (claude-usage-mode 1)
      (should (equal '(claude-usage-mode-line-string) global-mode-string))
      (claude-usage-mode -1)
      (should (null global-mode-string)))))

(ert-deftest claude-usage-record-error-keeps-the-last-good-values ()
  (let ((claude-usage--state '(:five-hour 68 :seven-day 53 :error nil))
        (claude-usage-mode-line-string ""))
    (claude-usage--record-error "boom")
    (should (equal 68 (plist-get claude-usage--state :five-hour)))
    (should (equal "boom" (plist-get claude-usage--state :error)))
    (should (equal " Claude 5h 68%% | 7d 53%%*" claude-usage-mode-line-string))))

(provide 'claude-usage-tests)

;;; claude-usage-tests.el ends here
