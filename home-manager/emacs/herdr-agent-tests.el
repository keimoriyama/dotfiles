;;; herdr-agent-tests.el --- Tests for herdr-agent -*- lexical-binding: t; -*-

(require 'ert)

(load (expand-file-name "herdr-agent.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst herdr-agent-tests--rule (make-string 40 ?─))

(ert-deftest herdr-agent-parses-a-cli-reply-into-an-alist ()
  "A JSON reply becomes an alist with lists for arrays and nil for null."
  (should (equal (herdr-agent--parse-json
                  "{\"result\":{\"agents\":[{\"name\":\"a\",\"cwd\":null}]}}\n")
                 '((result (agents ((name . "a") (cwd))))))))

(ert-deftest herdr-agent-returns-nil-for-output-that-is-not-json ()
  "Plain text output parses to nothing instead of signalling."
  (should (null (herdr-agent--parse-json "unknown option: --session")))
  (should (null (herdr-agent--parse-json ""))))

(ert-deftest herdr-agent-extracts-the-error-code ()
  "The error code of a failed command is available for dispatch."
  (should (equal (herdr-agent--error-code
                  "{\"error\":{\"code\":\"agent_not_ready\",\"message\":\"m\"},\"id\":\"x\"}")
                 "agent_not_ready")))

(ert-deftest herdr-agent-has-no-error-code-for-non-json-failures ()
  "A usage error printed as plain text has no code."
  (should (null (herdr-agent--error-code "unknown option: w1:p1"))))

(ert-deftest herdr-agent-error-message-prefers-the-server-message ()
  "A JSON error is reported with the command and the server's message."
  (should (equal (herdr-agent--error-message
                  '("agent" "start" "x")
                  "{\"error\":{\"code\":\"c\",\"message\":\"pane not found\"}}")
                 "herdr agent start: pane not found")))

(ert-deftest herdr-agent-error-message-falls-back-to-raw-output ()
  "A plain text failure is reported as printed."
  (should (equal (herdr-agent--error-message '("pane") "usage\n")
                 "herdr pane: usage")))

(ert-deftest herdr-agent-trims-the-input-box-and-footer ()
  "Everything from the input box down is dropped."
  (should (equal (herdr-agent-trim-footer
                  (string-join
                   (list "❯ question" "" "⏺ answer" ""
                         herdr-agent-tests--rule "❯ " herdr-agent-tests--rule
                         "  status line" "  model line")
                   "\n"))
                 "❯ question\n\n⏺ answer")))

(ert-deftest herdr-agent-keeps-screens-without-an-input-box ()
  "A screen with fewer than two rules only loses trailing blank lines."
  (should (equal (herdr-agent-trim-footer
                  (concat "$ ls\n" herdr-agent-tests--rule "\nfile\n\n\n"))
                 (concat "$ ls\n" herdr-agent-tests--rule "\nfile"))))

(ert-deftest herdr-agent-ignores-short-dashes-when-trimming ()
  "Markdown-ish short rules in the output are not taken for the input box."
  (let ((text "a\n────\nb\n────\nc"))
    (should (equal (herdr-agent-trim-footer text) text))))

(ert-deftest herdr-agent-applies-screen-functions-in-order ()
  "Screen functions run left to right on the text."
  (let ((herdr-agent-screen-functions
         (list #'upcase (lambda (text) (concat text "!")))))
    (should (equal (herdr-agent--apply-screen-functions "ok") "OK!"))))

(ert-deftest herdr-agent-shows-screens-unchanged-without-screen-functions ()
  "An empty function list leaves the screen as read."
  (let ((herdr-agent-screen-functions nil))
    (should (equal (herdr-agent--apply-screen-functions "raw") "raw"))))

(ert-deftest herdr-agent-names-an-agent-by-its-name ()
  "A named agent is shown by that name."
  (should (equal (herdr-agent--buffer-name
                  '((name . "reviewer") (agent . "claude") (pane_id . "w1:p1")))
                 "*herdr: reviewer*")))

(ert-deftest herdr-agent-names-an-unnamed-agent-by-kind-and-pane ()
  "An agent detected without a name falls back to kind and pane."
  (should (equal (herdr-agent--display-name '((agent . "codex") (pane_id . "w2:p3")))
                 "codex@w2:p3"))
  (should (equal (herdr-agent--display-name '((pane_id . "w2:p3")))
                 "agent@w2:p3")))

(ert-deftest herdr-agent-colours-statuses-by-urgency ()
  "Blocked agents stand out from working and idle ones."
  (should (eq (herdr-agent--status-face "blocked") 'herdr-agent-status-blocked))
  (should (eq (herdr-agent--status-face "working") 'herdr-agent-status-working)))

(ert-deftest herdr-agent-labels-a-missing-status-as-gone ()
  "An agent that left the list is labelled gone in a muted face."
  (let ((label (herdr-agent--status-label nil)))
    (should (equal label "gone"))
    (should (eq (get-text-property 0 'face label) 'herdr-agent-status-other))))

(ert-deftest herdr-agent-submits-text-by-typing-then-enter ()
  "Text, including newlines, is typed into the pane before Enter."
  (should (equal (herdr-agent--submit-commands "w1:p1" "line1\nline2")
                 '(("pane" "send-text" "w1:p1" "line1\nline2")
                   ("pane" "send-keys" "w1:p1" "enter")))))

(ert-deftest herdr-agent-submits-nothing-for-blank-text ()
  "Whitespace alone never reaches the agent."
  (should (null (herdr-agent--submit-commands "w1:p1" " \n\t"))))

(ert-deftest herdr-agent-lists-an-agent-with-its-working-directory ()
  "A list row shows the directory the agent's process is in."
  (let ((entry (herdr-agent--list-entry
                '((name . "a") (agent . "claude") (agent_status . "idle")
                  (pane_id . "w1:p1") (cwd . "/tmp")
                  (foreground_cwd . "/tmp/project")))))
    (should (equal (car entry) "w1:p1"))
    (should (equal (aref (cadr entry) 4) "/tmp/project"))))

(ert-deftest herdr-agent-lists-an-agent-without-a-directory ()
  "Missing kind and directory become empty cells rather than errors."
  (let ((row (cadr (herdr-agent--list-entry '((pane_id . "w1:p1"))))))
    (should (equal (aref row 1) ""))
    (should (equal (aref row 4) ""))))

(ert-deftest herdr-agent-quotes-a-region-with-its-location ()
  "A region is sent as a fenced block headed by file and lines."
  (should (equal (herdr-agent--region-context "~/a.el" 3 4 "(foo)\n(bar)\n")
                 "~/a.el:3-4\n```\n(foo)\n(bar)\n```\n")))

(ert-deftest herdr-agent-quotes-a-region-from-a-buffer-without-a-file ()
  "A region from a non-file buffer still carries its lines."
  (should (string-prefix-p "(buffer):1-1\n"
                           (herdr-agent--region-context nil 1 1 "x"))))

(ert-deftest herdr-agent-treats-working-and-blocked-agents-as-busy ()
  "Only the states in which the agent owns its screen count as busy."
  (should (herdr-agent--busy-p "working"))
  (should (herdr-agent--busy-p "blocked")))

(ert-deftest herdr-agent-treats-idle-and-unknown-agents-as-free ()
  "An idle agent, and one that is gone, are not busy."
  (should-not (herdr-agent--busy-p "idle"))
  (should-not (herdr-agent--busy-p nil)))

(ert-deftest herdr-agent-reads-history-from-an-idle-agent ()
  "A free agent is read with the scrolled history."
  (let ((herdr-agent-read-lines 200))
    (should (equal (herdr-agent--read-args "w1:p1" nil)
                   '("agent" "read" "w1:p1"
                     "--source" "recent-unwrapped" "--lines" "200")))))

(ert-deftest herdr-agent-reads-only-the-visible-screen-from-a-busy-agent ()
  "A busy agent is read without scrolling."
  (should (equal (nth 4 (herdr-agent--read-args "w1:p1" t)) "visible")))

(ert-deftest herdr-agent-reads-an-agent-it-has-not-seen-before ()
  "The first poll of an agent always reads its screen."
  (should (herdr-agent--needs-read-p nil '((state_change_seq . 7)
                                           (agent_status . "idle")))))

(ert-deftest herdr-agent-reads-an-agent-whose-state-sequence-moved ()
  "A turn shorter than the poll interval is caught by the sequence."
  (should (herdr-agent--needs-read-p '((state_change_seq . 7)
                                       (agent_status . "idle"))
                                     '((state_change_seq . 9)
                                       (agent_status . "idle")))))

(ert-deftest herdr-agent-keeps-reading-a-working-agent ()
  "A working agent is read again even while its sequence stands still."
  (should (herdr-agent--needs-read-p '((state_change_seq . 7)
                                       (agent_status . "working"))
                                     '((state_change_seq . 7)
                                       (agent_status . "working")))))

(ert-deftest herdr-agent-leaves-an-unchanged-idle-agent-alone ()
  "An idle agent that did not change is not read again."
  (should-not (herdr-agent--needs-read-p '((state_change_seq . 7)
                                           (agent_status . "idle"))
                                         '((state_change_seq . 7)
                                           (agent_status . "idle")))))

(ert-deftest herdr-agent-reads-nothing-for-an-agent-that-disappeared ()
  "An agent missing from the list has no screen to read."
  (should-not (herdr-agent--needs-read-p '((state_change_seq . 7)) nil)))

;;; herdr-agent-tests.el ends here
