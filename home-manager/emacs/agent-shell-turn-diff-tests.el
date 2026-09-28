;;; agent-shell-turn-diff-tests.el --- Tests for per-turn diffs of agent-shell -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(load (expand-file-name "agent-shell-turn-diff.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro agent-shell-turn-diff-tests--with-repo (&rest body)
  "Run BODY with `default-directory' in a fresh repository holding one commit."
  (declare (indent 0))
  `(let ((default-directory (file-name-as-directory
                             (make-temp-file "agent-shell-turn-diff" t))))
     (unwind-protect
         (progn
           (my-agent-shell-turn-diff--git "init" "-q")
           (my-agent-shell-turn-diff--git "config" "user.email" "t@example.com")
           (my-agent-shell-turn-diff--git "config" "user.name" "t")
           (write-region "old\n" nil "tracked.txt")
           (my-agent-shell-turn-diff--git "add" "tracked.txt")
           (my-agent-shell-turn-diff--git "commit" "-q" "-m" "init")
           ,@body)
       (delete-directory default-directory t))))

(defun agent-shell-turn-diff-tests--diff (before after)
  "Return the rendered diff text from BEFORE to AFTER."
  (with-current-buffer (my-agent-shell-turn-diff--render before after)
    (prog1 (buffer-string) (kill-buffer))))

(ert-deftest agent-shell-turn-diff-shows-an-edited-file ()
  "An edit between the two snapshots appears in the diff."
  (agent-shell-turn-diff-tests--with-repo
    (let ((before (my-agent-shell-turn-diff--snapshot)))
      (write-region "new\n" nil "tracked.txt")
      (let ((diff (agent-shell-turn-diff-tests--diff
                   before (my-agent-shell-turn-diff--snapshot))))
        (should (string-match-p "^-old$" diff))
        (should (string-match-p "^\\+new$" diff))))))

(ert-deftest agent-shell-turn-diff-includes-untracked-files ()
  "A file the agent creates shows up even though it is not staged."
  (agent-shell-turn-diff-tests--with-repo
    (let ((before (my-agent-shell-turn-diff--snapshot)))
      (write-region "hello\n" nil "created.txt")
      (should (string-match-p "created\\.txt"
                              (agent-shell-turn-diff-tests--diff
                               before (my-agent-shell-turn-diff--snapshot)))))))

(ert-deftest agent-shell-turn-diff-ignores-edits-made-before-the-turn ()
  "Uncommitted work that predates the turn stays out of its diff."
  (agent-shell-turn-diff-tests--with-repo
    (write-region "user\n" nil "tracked.txt")
    (let ((before (my-agent-shell-turn-diff--snapshot)))
      (write-region "hello\n" nil "created.txt")
      (should-not (string-match-p "tracked\\.txt"
                                  (agent-shell-turn-diff-tests--diff
                                   before (my-agent-shell-turn-diff--snapshot)))))))

(ert-deftest agent-shell-turn-diff-leaves-the-real-index-alone ()
  "Taking a snapshot stages nothing."
  (agent-shell-turn-diff-tests--with-repo
    (write-region "hello\n" nil "created.txt")
    (my-agent-shell-turn-diff--snapshot)
    (should (equal (my-agent-shell-turn-diff--git "diff" "--cached" "--name-only")
                   ""))))

(ert-deftest agent-shell-turn-diff-gives-equal-trees-when-nothing-changed ()
  "A turn that touches no file yields identical snapshots."
  (agent-shell-turn-diff-tests--with-repo
    (should (equal (my-agent-shell-turn-diff--snapshot)
                   (my-agent-shell-turn-diff--snapshot)))))

(ert-deftest agent-shell-turn-diff-skips-directories-outside-git ()
  "Outside a repository there is nothing to snapshot."
  (let ((default-directory (file-name-as-directory
                            (make-temp-file "agent-shell-turn-diff" t))))
    (unwind-protect
        (should (null (my-agent-shell-turn-diff--snapshot)))
      (delete-directory default-directory t))))

(ert-deftest agent-shell-turn-diff-remembers-only-turns-that-changed-files ()
  "A turn without edits keeps the previous turn's diff available."
  (agent-shell-turn-diff-tests--with-repo
    (with-temp-buffer
      (my-agent-shell-turn-diff--on-submit '(:data (:prompt "edit")))
      (write-region "new\n" nil "tracked.txt")
      (my-agent-shell-turn-diff--on-complete nil)
      (let ((edited my-agent-shell-turn-diff--last))
        (should (equal (plist-get edited :prompt) "edit"))
        (my-agent-shell-turn-diff--on-submit '(:data (:prompt "just talk")))
        (my-agent-shell-turn-diff--on-complete nil)
        (should (eq my-agent-shell-turn-diff--last edited))
        (should (null my-agent-shell-turn-diff--before))))))

(ert-deftest agent-shell-turn-diff-keeps-the-turn-start-on-steered-input ()
  "Input sent while a turn runs does not move the turn's starting point."
  (agent-shell-turn-diff-tests--with-repo
    (with-temp-buffer
      (my-agent-shell-turn-diff--on-submit '(:data (:prompt "first")))
      (let ((before my-agent-shell-turn-diff--before))
        (write-region "new\n" nil "tracked.txt")
        (my-agent-shell-turn-diff--on-submit '(:data (:prompt "steer")))
        (should (equal my-agent-shell-turn-diff--before before))
        (should (equal my-agent-shell-turn-diff--prompt "first"))))))

;;; agent-shell-turn-diff-tests.el ends here
