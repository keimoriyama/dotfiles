;;; agent-review-tests.el --- Tests for agent-review -*- lexical-binding: t; -*-

(require 'ert)

(load (expand-file-name "agent-review.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun agent-review-tests--lines (entries)
  "Return ENTRIES as (KIND OLD NEW TEXT) tuples, dropping headers."
  (cl-loop for e in entries
           when (memq (plist-get e :kind) '(ctx add del))
           collect (list (plist-get e :kind) (plist-get e :old)
                         (plist-get e :new) (plist-get e :text))))

(ert-deftest agent-review-parse-diff-numbers-lines-on-both-sides ()
  "Context lines advance both sides, removals only the old, additions only the new."
  (let ((entries (agent-review--parse-diff
                  (concat "diff --git a/src/a.el b/src/a.el\n"
                          "--- a/src/a.el\n"
                          "+++ b/src/a.el\n"
                          "@@ -10,3 +10,3 @@ (defun a ()\n"
                          " keep\n"
                          "-old\n"
                          "+new\n"
                          " tail\n"))))
    (should (equal (plist-get (car entries) :file) "src/a.el"))
    (should (equal (agent-review-tests--lines entries)
                   '((ctx 10 10 "keep")
                     (del 11 nil "old")
                     (add nil 11 "new")
                     (ctx 12 12 "tail"))))))

(ert-deftest agent-review-parse-diff-names-a-deleted-file-by-its-old-path ()
  "A file diffed against /dev/null on the new side keeps its old name."
  (let ((entries (agent-review--parse-diff
                  (concat "--- a/gone.txt\n"
                          "+++ /dev/null\n"
                          "@@ -1 +0,0 @@\n"
                          "-bye\n"))))
    (should (equal (plist-get (car entries) :file) "gone.txt"))
    (should (equal (agent-review-tests--lines entries) '((del 1 nil "bye"))))))

(ert-deftest agent-review-parse-diff-does-not-mistake-content-for-headers ()
  "Removed and added lines that look like ---/+++ headers stay content."
  (let ((entries (agent-review--parse-diff
                  (concat "--- a/x\n"
                          "+++ b/x\n"
                          "@@ -1 +1 @@\n"
                          "--- a\n"
                          "+++ b\n"
                          "\\ No newline at end of file\n"))))
    (should (equal (agent-review-tests--lines entries)
                   '((del 1 nil "-- a") (add nil 1 "++ b"))))))

(ert-deftest agent-review-parse-diff-uses-the-forced-file-name ()
  "Diffs of temporary files are attributed to the file under review."
  (let ((entries (agent-review--parse-diff
                  "--- /tmp/a\t2026\n+++ /tmp/b\t2026\n@@ -1 +1 @@\n-x\n+y\n"
                  "plan.md")))
    (should (cl-every (lambda (e) (equal (plist-get e :file) "plan.md")) entries))))

(ert-deftest agent-review-parse-diff-of-empty-text-is-empty ()
  "No changes yield no entries."
  (should (null (agent-review--parse-diff ""))))

(ert-deftest agent-review-hunk-heading-shows-the-project-relative-file ()
  "Each hunk identifies its file without dropping the section context."
  (let* ((entries (agent-review--parse-diff
                   (concat "--- a/flake.nix\n"
                           "+++ b/flake.nix\n"
                           "@@ -1 +1 @@ class config\n"
                           "-old\n"
                           "+new\n")))
         (hunk (cl-find 'hunk entries :key (lambda (e) (plist-get e :kind)))))
    (should (equal (agent-review--hunk-heading hunk)
                   "@@ -1 +1 @@ class config : ./flake.nix"))))

(ert-deftest agent-review-fontification-cache-follows-file-stamps ()
  "Unchanged files reuse fontification and changed files refresh it."
  (let ((s (agent-review--make-session :root "/r/"))
        (agent-review--fontify-cache (make-hash-table :test #'equal))
        (agent-review--fontify-render-cache (make-hash-table :test #'equal))
        (stamp 1)
        (stamp-calls 0)
        (calls 0))
    (cl-letf (((symbol-function 'agent-review--file-stamp)
               (lambda (_)
                 (cl-incf stamp-calls)
                 stamp))
              ((symbol-function 'agent-review--read-file)
               (lambda (_) "one\n"))
              ((symbol-function 'agent-review--fontify)
               (lambda (_ content)
                 (cl-incf calls)
                 (vconcat (agent-review--lines content)))))
      (should (equal (agent-review--fontified-line s "a" 1) "one"))
      (should (equal (agent-review--fontified-line s "a" 1) "one"))
      (should (= calls 1))
      (should (= stamp-calls 1))
      (setq stamp 2)
      (clrhash agent-review--fontify-render-cache)
      (should (equal (agent-review--fontified-line s "a" 1) "one"))
      (should (= calls 2))
      (should (= stamp-calls 2)))))

(ert-deftest agent-review-render-reuses-entries-until-invalidated ()
  "Comment-only redraws do not recompute the underlying diff."
  (let ((s (agent-review--make-session
            :id "cache" :mode 'git :root "/r/" :comments nil))
        (calls 0))
    (with-temp-buffer
      (agent-review-mode)
      (setq agent-review--session s)
      (cl-letf (((symbol-function 'agent-review--entries)
                 (lambda (_)
                   (cl-incf calls)
                   nil)))
        (agent-review--render)
        (agent-review--render)
        (should (= calls 1))
        (agent-review--invalidate-content-cache)
        (agent-review--render)
        (should (= calls 2))))))

(ert-deftest agent-review-git-entries-shows-only-unstaged-changes ()
  "A worktree review excludes committed and staged changes."
  (let ((root (make-temp-file "agent-review-repo-" t)) initial topic)
    (unwind-protect
        (progn
          (agent-review--git root "init" "--quiet")
          (with-temp-file (expand-file-name "value.txt" root) (insert "one\n"))
          (agent-review--git root "add" "value.txt")
          (agent-review--git root "-c" "user.name=Agent Review"
                             "-c" "user.email=agent-review@example.invalid"
                             "-c" "commit.gpgsign=false"
                             "commit" "--quiet" "-m" "initial")
          (setq initial (string-trim (agent-review--git root "rev-parse" "HEAD")))
          (agent-review--git root "checkout" "--quiet" "-b" "topic")
          (with-temp-file (expand-file-name "value.txt" root) (insert "two\n"))
          (agent-review--git root "add" "value.txt")
          (agent-review--git root "-c" "user.name=Agent Review"
                             "-c" "user.email=agent-review@example.invalid"
                             "-c" "commit.gpgsign=false"
                             "commit" "--quiet" "-m" "topic change")
          (setq topic (string-trim (agent-review--git root "rev-parse" "HEAD")))
          (with-temp-file (expand-file-name "value.txt" root) (insert "working\n"))
          (should (equal (agent-review-tests--lines
                          (agent-review--git-entries root))
                         '((del 1 nil "two") (add nil 1 "working"))))
          (agent-review--git root "add" "value.txt")
          (with-temp-file (expand-file-name "README.md" root)
            (insert "# New file\n"))
          (let ((entries (agent-review--git-entries root)))
            (should (equal (mapcar (lambda (e) (plist-get e :file)) entries)
                           '("README.md" "README.md")))
            (should (equal (agent-review-tests--lines entries)
                           '((add nil 1 "# New file")))))
          (should (equal (agent-review-tests--lines
                          (agent-review--git-entries root topic))
                         '((del 1 nil "one") (add nil 1 "two"))))
          (should (equal (agent-review-tests--lines
                          (agent-review--git-entries root initial))
                         '((add nil 1 "one"))))
          (should (equal (sort (mapcar #'cdr (agent-review--git-commits root))
                               #'string<)
                         (sort (list topic initial) #'string<))))
      (delete-directory root t))))

(ert-deftest agent-review-stages-only-the-selected-hunk ()
  "Staging one reviewed hunk leaves the other hunk visible."
  (let ((root (make-temp-file "agent-review-hunks-" t)))
    (unwind-protect
        (progn
          (agent-review--git root "init" "--quiet")
          (with-temp-file (expand-file-name "values.txt" root)
            (insert (mapconcat (lambda (n) (format "line %d" n))
                               (number-sequence 1 20) "\n") "\n"))
          (agent-review--git root "add" "values.txt")
          (agent-review--git root "-c" "user.name=Agent Review"
                             "-c" "user.email=agent-review@example.invalid"
                             "-c" "commit.gpgsign=false"
                             "commit" "--quiet" "-m" "initial")
          (with-temp-file (expand-file-name "values.txt" root)
            (let ((lines (mapcar (lambda (n) (format "line %d" n))
                                 (number-sequence 1 20))))
              (setf (nth 1 lines) "changed 2"
                    (nth 17 lines) "changed 18")
              (insert (string-join lines "\n") "\n")))
          (let* ((entries (agent-review--git-entries root))
                 (hunks (cl-remove-if-not
                         (lambda (e) (eq (plist-get e :kind) 'hunk)) entries))
                 (first (car hunks)))
            (should (= (length hunks) 2))
            (agent-review--git-input
             root (agent-review--git-hunk-patch
                   root "values.txt" (plist-get first :text))
             "apply" "--cached" "-")
            (let ((unstaged (agent-review--git-entries root))
                  (staged (agent-review--parse-diff
                           (agent-review--git root "diff" "--cached"))))
              (should (equal (mapcar (lambda (e) (plist-get e :text))
                                     (cl-remove-if-not
                                      (lambda (e) (memq (plist-get e :kind) '(add del)))
                                      unstaged))
                             '("line 18" "changed 18")))
              (should (equal (mapcar (lambda (e) (plist-get e :text))
                                     (cl-remove-if-not
                                      (lambda (e) (memq (plist-get e :kind) '(add del)))
                                      staged))
                             '("line 2" "changed 2"))))))
      (delete-directory root t))))

(defun agent-review-tests--comment (start end anchor)
  "Return a comment on START..END anchored on ANCHOR."
  (list :id "c_1" :file "plan.md" :start_line start :end_line end :anchor anchor))

(ert-deftest agent-review-reanchor-keeps-a-comment-whose-lines-are-unchanged ()
  "A comment stays put when its text is still where it was."
  (let ((c (agent-review--reanchor (agent-review-tests--comment 2 2 "b")
                                   ["a" "b" "c"])))
    (should (equal (plist-get c :start_line) 2))
    (should-not (plist-get c :drifted))))

(ert-deftest agent-review-reanchor-follows-lines-that-moved ()
  "Lines inserted above a commented range shift the comment down."
  (let ((c (agent-review--reanchor (agent-review-tests--comment 1 2 "b\nc")
                                   ["new" "new" "a" "b" "c"])))
    (should (equal (list (plist-get c :start_line) (plist-get c :end_line)) '(4 5)))
    (should-not (plist-get c :drifted))))

(ert-deftest agent-review-reanchor-prefers-the-nearest-of-several-matches ()
  "When the text repeats, the occurrence nearest the old line wins."
  (let ((c (agent-review--reanchor (agent-review-tests--comment 5 5 "x")
                                   ["x" "a" "a" "a" "a" "x" "a"])))
    (should (equal (plist-get c :start_line) 6))))

(ert-deftest agent-review-reanchor-marks-a-comment-drifted-when-its-text-is-gone ()
  "A comment whose text was rewritten keeps its lines and is marked drifted."
  (let ((c (agent-review--reanchor (agent-review-tests--comment 2 2 "b")
                                   ["a" "B" "c"])))
    (should (equal (plist-get c :start_line) 2))
    (should (plist-get c :drifted))))

(ert-deftest agent-review-reanchor-clears-drift-once-the-text-is-back ()
  "A drifted comment recovers when its text reappears."
  (let ((c (agent-review--reanchor
            (append (agent-review-tests--comment 2 2 "b") '(:drifted t))
            ["a" "b"])))
    (should-not (plist-get c :drifted))))

(ert-deftest agent-review-reanchor-ignores-a-range-longer-than-the-file ()
  "A range that no longer fits in the file drifts instead of erroring."
  (should (plist-get (agent-review--reanchor (agent-review-tests--comment 1 3 "a\nb\nc")
                                             ["a"])
                     :drifted)))

(ert-deftest agent-review-refresh-reanchors-comments-after-file-edits ()
  "A content refresh follows an unchanged anchor moved by an agent edit."
  (let* ((root (make-temp-file "agent-review-reanchor-" t))
         (file (expand-file-name "plan.md" root))
         (s (agent-review--make-session
             :root (file-name-as-directory root) :mode 'files :files (list file)
             :comments (list (agent-review-tests--comment 2 2 "target")))))
    (unwind-protect
        (progn
          (with-temp-file file (insert "new\na\ntarget\n"))
          (should (agent-review--reanchor-comments s))
          (should (= (plist-get (car (agent-review-session-comments s)) :start_line) 3))
          (should-not (agent-review--reanchor-comments s)))
      (delete-directory root t))))

(ert-deftest agent-review-git-watch-directories-include-untracked-files ()
  "Git refresh watches a new file's directory as well as the repository root."
  (let ((root (make-temp-file "agent-review-watch-" t)))
    (unwind-protect
        (progn
          (agent-review--git root "init" "--quiet")
          (make-directory (expand-file-name "docs" root))
          (with-temp-file (expand-file-name "docs/new.md" root) (insert "new\n"))
          (should (equal (sort (agent-review--git-watch-directories root) #'string<)
                         (sort (list (file-name-as-directory root)
                                     (file-name-as-directory
                                      (expand-file-name "docs" root)))
                               #'string<))))
      (delete-directory root t))))

(ert-deftest agent-review-finish-prompt-approves-a-round-without-comments ()
  "Finishing with no comments approves without changes."
  (should (equal (agent-review--finish-prompt nil "/r/z-ai/r.json")
                 "Review approved with no comments — no changes requested.\n")))

(ert-deftest agent-review-finish-prompt-approves-when-everything-is-resolved ()
  "Resolved comments alone approve the round."
  (should (string-prefix-p
           "Review approved. All comments are resolved"
           (agent-review--finish-prompt '((:id "c_1" :body "x" :resolved t))
                                        "/r/z-ai/r.json"))))

(ert-deftest agent-review-finish-prompt-hands-over-only-open-comments ()
  "Open comments are listed as JSON with the command for the next round."
  (let ((prompt (agent-review--finish-prompt
                 '((:id "c_1" :file "plan.md" :side "new" :start_line 3 :end_line 4
                    :anchor "a\nb" :body "Use SQS" :author "kei"
                    :replies ((:author "agent" :body "Done")))
                   (:id "c_2" :body "closed" :resolved t))
                 "/r/z-ai/r.json")))
    (should (string-prefix-p "The review finished with 1 unresolved comment.\n" prompt))
    (should (string-match-p (regexp-quote "\"id\":\"c_1\"") prompt))
    (should (string-match-p (regexp-quote "\"anchor\":\"a\\nb\"") prompt))
    (should (string-match-p (regexp-quote "\"replies\":[{\"author\":\"agent\",\"body\":\"Done\"}]") prompt))
    (should-not (string-match-p "c_2" prompt))
    (should-not (string-match-p "\"side\"" prompt))
    (should (string-match-p (regexp-quote "to\n\n  /r/z-ai/r.json\n\nas a JSON array") prompt))))

(ert-deftest agent-review-finish-prompt-counts-several-open-comments ()
  "The opening sentence uses the plural for several comments."
  (should (string-prefix-p
           "The review finished with 2 unresolved comments."
           (agent-review--finish-prompt '((:id "c_1" :body "x") (:id "c_2" :body "y"))
                                        "/r/z-ai/r.json"))))

(ert-deftest agent-review-finish-prompt-flags-removed-lines-and-drift ()
  "Comments on removed lines and drifted comments say so."
  (let ((prompt (agent-review--finish-prompt
                 '((:id "c_1" :file "a" :side "old" :start_line 1 :end_line 1
                    :body "x" :drifted t))
                 "/r/z-ai/r.json")))
    (should (string-match-p (regexp-quote "\"side\":\"old\"") prompt))
    (should (string-match-p (regexp-quote "\"drifted\":true") prompt))))

(ert-deftest agent-review-comment-json-does-not-store-rounds ()
  "Comments persist independently of review rounds."
  (let ((json (agent-review--comment-json
               '(:id "c_1" :body "x" :author "kei" :round 3))))
    (should-not (plist-member json :round))))

(ert-deftest agent-review-parse-replies-reads-ids-and-trimmed-bodies ()
  "Each reply object becomes an (ID . BODY) pair with the body trimmed."
  (should (equal (agent-review--parse-replies
                  "[{\"id\": \"c_1\", \"body\": \" 直しました\\n\"}, {\"id\": \"c_2\", \"body\": \"ok\"}]")
                 '(("c_1" . "直しました") ("c_2" . "ok")))))

(ert-deftest agent-review-parse-replies-accepts-an-empty-array ()
  "An agent with nothing to say still completes its turn."
  (should (null (agent-review--parse-replies "[]"))))

(ert-deftest agent-review-parse-replies-rejects-malformed-input ()
  "Truncated JSON, non-arrays and replies without a string body are errors."
  (should-error (agent-review--parse-replies "[{\"id\": \"c_1\""))
  (should-error (agent-review--parse-replies "{\"id\": \"c_1\", \"body\": \"x\"}"))
  (should-error (agent-review--parse-replies "[{\"id\": \"c_1\", \"body\": 3}]")))

(ert-deftest agent-review-add-replies-appends-to-the-matching-comment ()
  "Replies go after existing ones; unknown ids and empty bodies are dropped."
  (let* ((comments (list (list :id "c_1" :body "x"
                               :replies (list (list :author "kei" :body "why?")))
                         (list :id "c_2" :body "y")))
         (result (agent-review--add-replies
                  comments '(("c_1" . "done") ("c_9" . "lost") ("c_2" . "")) "agent")))
    (should (equal (plist-get (car result) :replies)
                   '((:author "kei" :body "why?") (:author "agent" :body "done"))))
    (should-not (plist-get (cadr result) :replies))
    (should (equal (length (plist-get (car comments) :replies)) 1))))

(ert-deftest agent-review-session-id-ignores-the-order-of-files ()
  "The same files in another order resume the same review."
  (should (equal (agent-review--session-id 'files "/r/" '("/r/a" "/r/b") nil)
                 (agent-review--session-id 'files "/r/" '("/r/b" "/r/a") nil))))

(ert-deftest agent-review-session-id-separates-git-branches ()
  "Each branch of a repository gets its own git review."
  (should-not (equal (agent-review--session-id 'git "/r/" nil "main")
                     (agent-review--session-id 'git "/r/" nil "topic"))))

(ert-deftest agent-review-session-id-separates-selected-commits ()
  "Comments on one selected commit do not leak into another review."
  (should-not (equal (agent-review--session-id 'git "/r/" nil nil "aaaa")
                     (agent-review--session-id 'git "/r/" nil nil "bbbb"))))

(ert-deftest agent-review-session-layout-stays-compatible-across-reloads ()
  "Adding commit selection does not corrupt sessions created by older code."
  (let* ((buffer (generate-new-buffer " *agent-review-legacy*"))
         (legacy (record 'agent-review-session "old" 'git "/r/" nil
                         1 "reviewing" nil nil nil nil nil buffer))
         (interim (record 'agent-review-session "interim" 'git "/r/" nil
                          1 "reviewing" nil nil nil nil nil "deadbeef" buffer)))
    (unwind-protect
        (progn
          (should (eq (agent-review-session-buffer legacy) buffer))
          (should-not (agent-review--session-revision legacy))
          (agent-review--normalize-session-layout interim)
          (should (eq (agent-review-session-buffer interim) buffer))
          (should (equal (agent-review--session-revision interim) "deadbeef")))
      (kill-buffer buffer))))

(ert-deftest agent-review-mode-binds-staging-and-commit-selection ()
  "A Git review can stage with s and open the commit picker with S."
  (should (eq (keymap-lookup agent-review-mode-map "s") #'agent-review-stage))
  (should (eq (keymap-lookup agent-review-mode-map "S")
              #'agent-review-select-commit)))

(ert-deftest agent-review-lines-drops-only-the-final-newline ()
  "A trailing newline ends the last line; blank lines inside are kept."
  (should (equal (agent-review--lines "a\n\nb\n") '("a" "" "b")))
  (should (equal (agent-review--lines "a") '("a")))
  (should (null (agent-review--lines ""))))

(ert-deftest agent-review-read-file-ignores-directories ()
  "Gitlinks are directories in the worktree and have no file contents to read."
  (let ((dir (make-temp-file "agent-review-gitlink-" t)))
    (unwind-protect
        (should (null (agent-review--read-file dir)))
      (delete-directory dir))))

(ert-deftest agent-review-render-hides-resolved-comments ()
  "Resolved comments stay in the session but disappear from the review buffer."
  (let ((s (agent-review--make-session
            :id "comments" :mode 'git :root "/r/"
            :comments '((:id "open" :body "SHOW-OPEN" :author "kei")
                        (:id "closed" :body "HIDE-RESOLVED" :author "kei"
                         :resolved t)))))
    (with-temp-buffer
      (agent-review-mode)
      (setq agent-review--session s)
      (cl-letf (((symbol-function 'agent-review--entries) (lambda (_s) nil)))
        (agent-review--render))
      (should (string-match-p "SHOW-OPEN" (buffer-string)))
      (should-not (string-match-p "HIDE-RESOLVED" (buffer-string))))))

(ert-deftest agent-review-save-and-load-round-trip ()
  "A saved session reads back with its comments, replies and snapshots."
  (let* ((agent-review-directory (make-temp-file "agent-review-test-" t))
         (s (agent-review--make-session
             :id "abc123" :mode 'files :root "/r/" :files '("/r/plan.md")
             :revision "0123456789abcdef"
             :round 2 :state "agent"
             :comments '((:id "c_1" :file "plan.md" :side "new" :start_line 1
                          :end_line 1 :anchor "a" :body "日本語" :author "kei"
                          :resolved t
                          :replies ((:author "agent" :body "ok"))))
             :snapshot '(("plan.md" . "a\n"))
             :prev-snapshot nil)))
    (unwind-protect
        (progn
          (agent-review--save s)
          (should (file-exists-p
                   (expand-file-name "r/abc123/review.json" agent-review-directory)))
          (should (equal (agent-review--reply-file s)
                         (expand-file-name "r/abc123/replies.json"
                                           agent-review-directory)))
          (let ((l (agent-review--load "abc123")))
            (should (eq (agent-review-session-mode l) 'files))
            (should (equal (agent-review-session-files l) '("/r/plan.md")))
            (should (equal (agent-review-session-revision l) "0123456789abcdef"))
            (should (equal (agent-review-session-round l) 2))
            (should (equal (agent-review-session-comments l)
                           (agent-review-session-comments s)))
            (should (equal (agent-review-session-snapshot l) '(("plan.md" . "a\n"))))
            (should (null (agent-review-session-prev-snapshot l)))
            (should (null (agent-review-session-approved l)))))
      (delete-directory agent-review-directory t))))

(ert-deftest agent-review-load-of-an-unknown-session-is-nil ()
  "Loading a session that was never saved returns nil."
  (let ((agent-review-directory (make-temp-file "agent-review-test-" t)))
    (unwind-protect
        (should (null (agent-review--load "ffffff")))
      (delete-directory agent-review-directory))))

;;; agent-review-tests.el ends here
