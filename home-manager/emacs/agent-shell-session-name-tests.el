;;; agent-shell-session-name-tests.el --- Tests for agent-shell buffer naming -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))

(load (expand-file-name "agent-shell-session-name.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest agent-shell-session-name-names-buffer-after-agent-and-project ()
  "A buffer name tells which agent runs in which project."
  (should (equal (my-agent-shell-session-name-format "Claude" "dotfiles")
                 "Claude@dotfiles")))

(ert-deftest agent-shell-session-name-keeps-a-short-summary-intact ()
  "A summary within the length budget is used as written."
  (should (equal (my-agent-shell-session-name--shorten "バッファ名の変更")
                 "バッファ名の変更")))

(ert-deftest agent-shell-session-name-truncates-an-overlong-summary ()
  "A summary past the budget is cut and marked as cut."
  (let ((my-agent-shell-session-name-summary-length 5))
    (should (equal (my-agent-shell-session-name--shorten "あいうえおかきくけこ")
                   "あいうえお…"))))

(ert-deftest agent-shell-session-name-uses-only-the-first-line ()
  "A multi-line reply contributes just its first line."
  (should (equal (my-agent-shell-session-name--shorten "  要約  \n余計な説明\n")
                 "要約")))

(ert-deftest agent-shell-session-name-rejects-an-empty-summary ()
  "Blank output produces no summary rather than a trailing space."
  (should (null (my-agent-shell-session-name--shorten "  \n\n")))
  (should (null (my-agent-shell-session-name--shorten nil))))

(ert-deftest agent-shell-session-name-appends-a-summary-to-the-base-name ()
  "The summary follows the agent and project."
  (should (equal (my-agent-shell-session-name--compose "Claude@dotfiles" "要約")
                 "Claude@dotfiles 要約")))

(ert-deftest agent-shell-session-name-keeps-the-base-name-without-a-summary ()
  "A missing summary leaves the buffer name unchanged."
  (should (equal (my-agent-shell-session-name--compose "Claude@dotfiles" nil)
                 "Claude@dotfiles")))

(ert-deftest agent-shell-session-name-caps-the-prompt-sent-to-the-summariser ()
  "A long prompt is trimmed before it reaches the summariser."
  (let* ((my-agent-shell-session-name-prompt-limit 10)
         (prompt (make-string 100 ?あ))
         (built (my-agent-shell-session-name--summary-prompt prompt)))
    (should (string-suffix-p (make-string 10 ?あ) built))
    (should-not (string-match-p (make-string 11 ?あ) built))))

(ert-deftest agent-shell-session-name-renames-on-the-first-prompt-only ()
  "The first submitted prompt names the buffer; the subscription then ends."
  (let (renamed unsubscribed)
    (cl-letf (((symbol-function 'agent-shell-unsubscribe)
               (lambda (&rest args) (setq unsubscribed (plist-get args :subscription))))
              ((symbol-function 'shell-maker-set-buffer-name)
               (lambda (_buffer name) (setq renamed name)))
              ((symbol-function 'my-agent-shell-session-name--summarise)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (setq my-agent-shell-session-name--base "Claude@dotfiles")
        (my-agent-shell-session-name--on-first-prompt
         (current-buffer) 7 '((:event . input-submitted)
                              (:data . ((:prompt . "セッション名を変えたい")))))
        (should (equal renamed "Claude@dotfiles セッション名を変えたい"))
        (should (equal unsubscribed 7))))))

(ert-deftest agent-shell-session-name-ignores-an-empty-prompt ()
  "An empty prompt leaves the buffer name alone."
  (let (renamed)
    (cl-letf (((symbol-function 'agent-shell-unsubscribe) #'ignore)
              ((symbol-function 'shell-maker-set-buffer-name)
               (lambda (_buffer name) (setq renamed name)))
              ((symbol-function 'my-agent-shell-session-name--summarise)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (setq my-agent-shell-session-name--base "Claude@dotfiles")
        (my-agent-shell-session-name--on-first-prompt
         (current-buffer) 7 '((:event . input-submitted)
                              (:data . ((:prompt . "")))))
        (should (null renamed))))))

;;; agent-shell-session-name-tests.el ends here
