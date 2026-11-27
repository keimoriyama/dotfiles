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

(ert-deftest agent-shell-tool-output-limit-keeps-output-exactly-at-the-cap ()
  "Output with exactly head plus tail lines is not shortened."
  (let ((my-agent-shell-tool-output-head-lines 2)
        (my-agent-shell-tool-output-tail-lines 1))
    (should (equal (my-agent-shell-tool-output-limit-lines "a\nb\nc")
                   "a\nb\nc"))
    (should-not (equal (my-agent-shell-tool-output-limit-lines "a\nb\nc\nd")
                       "a\nb\nc\nd"))))

(ert-deftest agent-shell-tool-output-limit-closes-a-fence-closed-in-the-omission ()
  "A code block whose closing fence is omitted is closed after the note,
so plain text in the tail (such as a diff) is not rendered as code."
  (let ((my-agent-shell-tool-output-head-lines 2)
        (my-agent-shell-tool-output-tail-lines 2))
    (should (equal (my-agent-shell-tool-output-limit-lines
                    "```\n1\n2\n```\n-old\n+new")
                   "```\n1\n\n... [2 lines omitted] ...\n\n```\n-old\n+new"))))

(ert-deftest agent-shell-tool-output-limit-opens-a-fence-opened-in-the-omission ()
  "A code block whose opening fence is omitted is reopened after the note."
  (let ((my-agent-shell-tool-output-head-lines 2)
        (my-agent-shell-tool-output-tail-lines 2))
    (should (equal (my-agent-shell-tool-output-limit-lines
                    "a\nb\nc\n```\n1\n```")
                   "a\nb\n\n... [2 lines omitted] ...\n\n```\n1\n```"))))

(ert-deftest agent-shell-tool-output-limit-treats-shorter-runs-as-fence-content ()
  "Three-backtick lines inside a four-backtick block do not add a fence."
  (let ((my-agent-shell-tool-output-head-lines 3)
        (my-agent-shell-tool-output-tail-lines 2))
    (should (equal (my-agent-shell-tool-output-limit-lines
                    "````\n# doc\n```\ncode\n```\nend\n````")
                   "````\n# doc\n```\n\n... [2 lines omitted] ...\n\nend\n````"))))

(ert-deftest agent-shell-tool-output-limit-closes-a-long-fence-with-its-own-length ()
  "A four-backtick block closed in the omission is closed with four backticks."
  (let ((my-agent-shell-tool-output-head-lines 2)
        (my-agent-shell-tool-output-tail-lines 1))
    (should (equal (my-agent-shell-tool-output-limit-lines
                    "````\n```\n1\n````\nplain")
                   "````\n```\n\n... [2 lines omitted] ...\n\n````\nplain"))))

(provide 'agent-shell-tool-output-limit-tests)
;;; agent-shell-tool-output-limit-tests.el ends here
