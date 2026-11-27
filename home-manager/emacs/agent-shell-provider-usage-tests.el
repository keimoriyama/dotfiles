;;; agent-shell-provider-usage-tests.el --- Tests for provider usage commands -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))

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

(defmacro agent-shell-provider-usage-tests--with-rollouts (lines &rest body)
  "Run BODY with a Codex sessions directory holding one rollout of LINES."
  (declare (indent 1))
  `(let ((directory (make-temp-file "codex-sessions-" t)))
     (unwind-protect
         (let ((my-agent-shell-codex-sessions-directory directory))
           (with-temp-file (expand-file-name "rollout.jsonl" directory)
             (dolist (line ,lines) (insert line "\n")))
           ,@body)
       (delete-directory directory t))))

(ert-deftest agent-shell-provider-usage-reads-a-codex-window-next-to-a-null-one ()
  "A window Codex reports as null is skipped; the other one is still shown."
  (agent-shell-provider-usage-tests--with-rollouts
      '("{\"payload\":{\"rate_limits\":{\"primary\":{\"used_percent\":10,\"window_minutes\":300,\"resets_at\":null},\"secondary\":null}}}")
    (should (equal (my-agent-shell--read-codex-rate-limits)
                   '((:label "5h" :used 10 :reset nil))))))

(ert-deftest agent-shell-provider-usage-skips-a-null-codex-snapshot ()
  "A newer event without a snapshot falls back to the older one that has it."
  (agent-shell-provider-usage-tests--with-rollouts
      '("{\"payload\":{\"rate_limits\":{\"primary\":{\"used_percent\":10,\"window_minutes\":300,\"resets_at\":1}}}}"
        "{\"payload\":{\"rate_limits\":null}}")
    (should (equal (my-agent-shell--read-codex-rate-limits)
                   '((:label "5h" :used 10 :reset 1))))))

(ert-deftest agent-shell-provider-usage-searches-only-the-newest-codex-rollouts ()
  "Limits older than the scanned rollouts are not searched for."
  (let ((directory (make-temp-file "codex-sessions-" t))
        (my-agent-shell--codex-rollouts-scanned 1))
    (unwind-protect
        (let ((my-agent-shell-codex-sessions-directory directory)
              (old (expand-file-name "old.jsonl" directory))
              (new (expand-file-name "new.jsonl" directory)))
          (with-temp-file old
            (insert "{\"payload\":{\"rate_limits\":{\"primary\":{\"used_percent\":10,\"window_minutes\":300}}}}\n"))
          (with-temp-file new (insert "{\"payload\":{}}\n"))
          (set-file-times old (time-subtract nil 3600))
          (should-not (my-agent-shell--read-codex-rate-limits))
          (setq my-agent-shell--codex-rollouts-scanned 2)
          (should (my-agent-shell--read-codex-rate-limits)))
      (delete-directory directory t))))

(ert-deftest agent-shell-provider-usage-does-not-rescan-after-a-failed-read ()
  "A read that fails is not repeated on every redisplay until the TTL passes."
  (let ((my-agent-shell--codex-rate-limit-cache nil)
        (reads 0))
    (cl-letf (((symbol-function 'agent-shell-get-config)
               (lambda (_) '((:identifier . codex))))
              ((symbol-function 'my-agent-shell--read-codex-rate-limits)
               (lambda () (cl-incf reads) (error "Broken rollout"))))
      (with-temp-buffer
        (dotimes (_ 5)
          (should-not (my-agent-shell-rate-limit-mode-line))))
      (should (= reads 1)))))

(ert-deftest agent-shell-provider-usage-click-acts-on-the-clicked-window ()
  "A mode-line click sends the command for the clicked window's agent,
even when another window is selected; a key press uses the current buffer."
  (let ((codex (generate-new-buffer "codex"))
        (claude (generate-new-buffer "claude"))
        sent-from)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-get-config)
                   (lambda (buffer)
                     `((:identifier . ,(if (eq buffer codex) 'codex 'claude-code)))))
                  ((symbol-function 'agent-shell-prompt-queue)
                   (lambda (prompt) (setq sent-from (cons (current-buffer) prompt)))))
          (save-window-excursion
            (delete-other-windows)
            (switch-to-buffer claude)
            (let ((other (split-window)))
              (set-window-buffer other codex)
              (my-agent-shell-show-provider-usage
               `(mouse-1 (,other mode-line (0 . 0) 0)))
              (should (equal sent-from (cons codex "/status")))
              (my-agent-shell-show-provider-usage ?\C-u)
              (should (equal sent-from (cons claude "/usage"))))))
      (kill-buffer codex)
      (kill-buffer claude))))

(ert-deftest agent-shell-provider-usage-handles-missing-codex-rollouts ()
  "A missing Codex sessions directory produces no mode-line data."
  (let ((my-agent-shell-codex-sessions-directory
         (make-temp-name temporary-file-directory)))
    (should-not (my-agent-shell--read-codex-rate-limits))))

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
