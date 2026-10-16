;;; agent-shell-tool-output-limit-tests.el --- Tests for the tool output line cap -*- lexical-binding: t; -*-

(require 'ert)

(load (expand-file-name "agent-shell-tool-output-limit.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest agent-shell-tool-output-limit-keeps-short-output ()
  "Output within the cap comes back unchanged."
  (let ((my-agent-shell-tool-output-head-lines 2)
        (my-agent-shell-tool-output-tail-lines 1))
    (should (equal (my-agent-shell-tool-output-limit-lines "a\nb\nc")
                   "a\nb\nc"))))

(ert-deftest agent-shell-tool-output-limit-keeps-head-and-tail ()
  "Long output keeps its head and tail and reports the omitted lines."
  (let ((my-agent-shell-tool-output-head-lines 2)
        (my-agent-shell-tool-output-tail-lines 1))
    (should (equal (my-agent-shell-tool-output-limit-lines "```\n1\n2\n3\n```")
                   "```\n1\n\n... [2 lines omitted] ...\n\n```"))))

(provide 'agent-shell-tool-output-limit-tests)
;;; agent-shell-tool-output-limit-tests.el ends here
