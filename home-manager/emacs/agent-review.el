;;; agent-review.el --- Review agent plans and diffs in Emacs buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; A review loop for coding agents that stays inside Emacs, modelled on
;; crit (https://github.com/tomasz-tomczyk/crit).
;;
;; The reviewer opens a review with M-x agent-review, comments on lines or
;; ranges and finishes the round with C-c C-c, which sends the unresolved
;; comments to an agent-shell buffer (or the kill ring when there is none).
;; The agent edits the files and writes its replies to the session directory;
;; once that file appears, the replies are added to the comments and the next
;; round starts.
;;
;; Two kinds of review:
;; - files: the given files with line numbers.  `d' toggles a diff against
;;   what the reviewer saw in the previous round.
;; - git: with no files, unstaged and untracked files in the working tree.
;;
;; A session is keyed by its files (or by repository and branch), kept in
;; `agent-review-directory', and survives an Emacs restart.

;;; Code:

(require 'cl-lib)
(require 'crm)
(require 'diff-mode)
(require 'filenotify)
(require 'project)
(require 'subr-x)
(require 'text-property-search)

(defgroup agent-review nil
  "Review coding agent output in Emacs buffers."
  :group 'tools)

(defcustom agent-review-directory (expand-file-name "~/.local/agent-review/")
  "Directory holding reviews grouped by project and session."
  :type 'directory)

;; Move the previous default when this file is reloaded in a running Emacs.
(when (equal (expand-file-name agent-review-directory)
             (expand-file-name (locate-user-emacs-file "agent-review/")))
  (setq agent-review-directory (expand-file-name "~/.local/agent-review/")))

(defcustom agent-review-author
  (if (string-empty-p user-full-name) user-login-name user-full-name)
  "Author name put on the reviewer's comments and replies."
  :type 'string)

(defun agent-review--list-of-strings-p (value)
  "Return non-nil when VALUE is a list containing only strings."
  (and (proper-list-p value) (cl-every #'stringp value)))

(defcustom agent-review-fontify-max-size 300000
  "Files larger than this many characters are shown without fontification."
  :type 'integer)

(defcustom agent-review-hidden-file-regexps nil
  "Regexps matching project-relative file names hidden initially.
The selection made with `agent-review-select-files' overrides these patterns
in the current review buffer."
  :type '(repeat regexp)
  :safe #'agent-review--list-of-strings-p)

(defcustom agent-review-hidden-file-globs nil
  "Shell wildcard patterns matching project-relative file names hidden initially.
For example, \"*.out\", \"logs/*\" or \"Cargo.lock\".  No escaping is needed."
  :type '(repeat string)
  :safe #'agent-review--list-of-strings-p)

(defcustom agent-review-git-poll-interval 2
  "Seconds between checks of the working tree shown in a visible git review."
  :type 'number)

(defface agent-review-file-header '((t :inherit diff-file-header :extend t))
  "Face for file headers.")

(defface agent-review-line-number '((t :inherit line-number))
  "Face for line numbers.")

(defface agent-review-hunk '((t :inherit diff-hunk-header :extend t))
  "Face for hunk headers.")

(defface agent-review-added '((t :inherit diff-added))
  "Face for added lines.")

(defface agent-review-removed '((t :inherit diff-removed))
  "Face for removed lines.")

(defface agent-review-comment '((t :inherit font-lock-string-face))
  "Face for open comments.")

(defface agent-review-resolved '((t :inherit shadow))
  "Face for resolved comments.")

(cl-defstruct (agent-review-session
               (:constructor agent-review--make-session)
               (:copier nil))
  id mode root files (round 1) (state "reviewing") comments
  snapshot prev-snapshot prompt approved buffer revision)

(defun agent-review--session-revision (s)
  "Return S's selected revision, tolerating sessions made before that slot existed."
  (condition-case nil
      (agent-review-session-revision s)
    (args-out-of-range nil)))

(defvar agent-review--sessions (make-hash-table :test #'equal)
  "Live sessions by id.")

(defun agent-review--normalize-session-layout (s)
  "Repair S created while the revision slot preceded the buffer slot."
  (when (and (> (length s) 13)
             (or (bufferp (aref s 13))
                 (and (stringp (aref s 12)) (null (aref s 13)))))
    (let ((revision (aref s 12)))
      (aset s 12 (aref s 13))
      (aset s 13 revision)))
  s)

;; Preserve live sessions across evaluation of a newer version of this file.
(maphash (lambda (_id s) (agent-review--normalize-session-layout s))
         agent-review--sessions)

(defvar agent-review--reply-watches (make-hash-table :test #'equal)
  "Watches for the agent's reply file, as (DESCRIPTOR . TIMER) by session id.")

(defvar-local agent-review--session nil
  "Session shown in the current review buffer.")

(defvar-local agent-review--round-diff nil
  "Non-nil when a files review shows changes since the previous round.")

(defvar-local agent-review--file-overrides nil
  "Alist of project-relative file names and their selected visibility.")

(defvar agent-review--glob-regexp-cache (make-hash-table :test #'equal)
  "Cached regexps converted from shell wildcard patterns.")

(defvar-local agent-review--watches nil
  "File notification descriptors of the current review buffer.")

(defvar-local agent-review--refresh-timer nil
  "Pending debounced refresh of the current review buffer.")

(defvar-local agent-review--poll-timer nil
  "Repeating timer checking the working tree of the current git review.")

(defvar-local agent-review--git-state nil
  "Working-tree state of the current git review at its last check.")

(defvar-local agent-review--fontify-cache nil
  "Fontified lines cached by absolute file name and file stamp.")

(defvar agent-review--fontify-render-cache nil
  "Fontified lines by file, dynamically bound for one render.")

(defvar-local agent-review--entries-cache nil
  "Entries from the most recent content refresh.")

(defvar-local agent-review--entries-cache-valid nil
  "Non-nil when `agent-review--entries-cache' may be reused.")

;;;; Pure helpers

(defun agent-review--lines (content)
  "Split CONTENT into lines, ignoring the final newline."
  (let ((lines (split-string content "\n")))
    (if (and lines (string-empty-p (car (last lines))))
        (butlast lines)
      lines)))

(defun agent-review--obj (&rest kvs)
  "Return the plist KVS without the pairs whose value is nil."
  (let (out)
    (while kvs
      (let ((k (pop kvs)) (v (pop kvs)))
        (when v (setq out (nconc out (list k v))))))
    out))

(defun agent-review--session-id (mode root files branch &optional revision)
  "Return the session id for MODE over ROOT, FILES, BRANCH and REVISION."
  (substring
   (secure-hash 'sha1
                (string-join
                 (cons (symbol-name mode)
                       (if (eq mode 'git)
                           (list root (or revision branch ""))
                         (sort (copy-sequence files) #'string<)))
                 "\0"))
   0 12))

(defun agent-review--diff-path (name)
  "Return the file path named by a ---/+++ header NAME, or nil for /dev/null."
  (let ((name (replace-regexp-in-string "\t.*\\'" "" name)))
    (cond ((equal name "/dev/null") nil)
          ((string-match-p "\\`[ab]/" name) (substring name 2))
          (t name))))

(defun agent-review--parse-diff (text &optional file)
  "Parse the unified diff TEXT into a list of entries.
Each entry is a plist whose :kind is `file', `hunk', `ctx', `add' or `del'.
Line entries carry :file, :text and the :old and/or :new line number.
FILE, when non-nil, names every file instead of the diff headers."
  (let ((old-left 0) (new-left 0) old new old-name cur entries)
    (dolist (line (split-string text "\n"))
      (cond
       ((or (> old-left 0) (> new-left 0))
        (let ((sign (if (string-empty-p line) ?\s (aref line 0)))
              (body (if (string-empty-p line) "" (substring line 1))))
          (pcase sign
            (?\s
             (push (list :kind 'ctx :file cur :old old :new new :text body) entries)
             (cl-incf old) (cl-incf new) (cl-decf old-left) (cl-decf new-left))
            (?-
             (push (list :kind 'del :file cur :old old :text body) entries)
             (cl-incf old) (cl-decf old-left))
            (?+
             (push (list :kind 'add :file cur :new new :text body) entries)
             (cl-incf new) (cl-decf new-left)))))
       ((string-match
         "\\`@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
         line)
        (setq old (string-to-number (match-string 1 line))
              old-left (if (match-string 2 line) (string-to-number (match-string 2 line)) 1)
              new (string-to-number (match-string 3 line))
              new-left (if (match-string 4 line) (string-to-number (match-string 4 line)) 1))
        (push (list :kind 'hunk :file cur :text line) entries))
       ((string-prefix-p "--- " line)
        (setq old-name (agent-review--diff-path (substring line 4))))
       ((string-prefix-p "+++ " line)
        (setq cur (or file (agent-review--diff-path (substring line 4)) old-name))
        (push (list :kind 'file :file cur) entries))))
    (nreverse entries)))

(defun agent-review--reanchor (comment lines)
  "Return COMMENT moved to where its anchor text now sits in LINES.
LINES is a vector of the file's current lines.  Among several matches the
one nearest the old position wins.  Without any match the comment keeps
its lines and is marked drifted."
  (let ((anchor (plist-get comment :anchor))
        (start (plist-get comment :start_line))
        (c (copy-sequence comment)))
    (if (null anchor)
        comment
      (let* ((want (split-string anchor "\n"))
             (n (length want))
             best)
        (dotimes (i (max 0 (1+ (- (length lines) n))))
          (when (cl-loop for w in want for j from i
                         always (equal w (aref lines j)))
            (when (or (null best) (< (abs (- (1+ i) start)) (abs (- best start))))
              (setq best (1+ i)))))
        (if best
            (thread-first c
                          (plist-put :start_line best)
                          (plist-put :end_line (+ best n -1))
                          (plist-put :drifted nil))
          (plist-put c :drifted t))))))

(defun agent-review--unresolved (comments)
  "Return the COMMENTS not yet resolved."
  (cl-remove-if (lambda (c) (plist-get c :resolved)) comments))

(defun agent-review--agent-json (comment)
  "Return COMMENT as the object handed to the agent."
  (agent-review--obj
   :id (plist-get comment :id)
   :file (plist-get comment :file)
   :side (and (equal (plist-get comment :side) "old") "old")
   :start_line (plist-get comment :start_line)
   :end_line (plist-get comment :end_line)
   :anchor (plist-get comment :anchor)
   :body (plist-get comment :body)
   :drifted (and (plist-get comment :drifted) t)
   :replies (and (plist-get comment :replies)
                 (vconcat (mapcar (lambda (r)
                                    (list :author (plist-get r :author)
                                          :body (plist-get r :body)))
                                  (plist-get comment :replies))))))

(defun agent-review--comments-json (comments)
  "Return COMMENTS as a JSON array with one comment per line."
  (with-temp-buffer
    (insert "[")
    (cl-loop for c in comments
             for first = t then nil
             do (insert (if first "\n  " ",\n  "))
             (json-insert (agent-review--agent-json c)))
    (insert "\n]")
    (buffer-string)))

(defun agent-review--finish-prompt (comments reply-file)
  "Return the agent's instructions after a round with COMMENTS.
REPLY-FILE is where the agent writes its replies to start the next round."
  (let* ((open (agent-review--unresolved comments))
         (n (length open)))
    (cond
     ((null comments)
      "Review approved with no comments — no changes requested.\n")
     ((null open)
      "Review approved. All comments are resolved — proceed with implementation.\n")
     (t
      (concat
       (if (= n 1)
           "The review finished with 1 unresolved comment."
         (format "The review finished with %d unresolved comments." n))
       "\n\n" (agent-review--comments-json open) "\n\n"
       "Address each comment by editing the files. "
       "`anchor` is the commented text when the comment was made; "
       "`side: \"old\"` marks removed lines and `drifted: true` "
       "means the commented text has since been rewritten.\n\n"
       "Then write one reply per comment, explaining what you did, to\n\n  "
       reply-file "\n\n"
       "as a JSON array: [{\"id\": \"c_...\", \"body\": \"...\"}]. "
       "Write that file last: the next review round starts as soon as it appears. "
       "Put your explanations in these replies, not in chat, and do not "
       "declare comments resolved; the reviewer decides.\n")))))

(defun agent-review--parse-replies (text)
  "Return the replies in TEXT as (ID . BODY) pairs.
Signal an error unless TEXT is a JSON array of objects with string
`id' and `body' members."
  (let ((data (json-parse-string text :object-type 'plist :array-type 'list
                                 :null-object nil :false-object nil)))
    (unless (listp data)
      (error "Replies are not a JSON array"))
    (mapcar (lambda (o)
              (let ((id (and (listp o) (plist-get o :id)))
                    (body (and (listp o) (plist-get o :body))))
                (unless (and (stringp id) (stringp body))
                  (error "Reply without a string id and body: %S" o))
                (cons id (string-trim body))))
            data)))

(defun agent-review--add-replies (comments replies author)
  "Return COMMENTS with REPLIES, (ID . BODY) pairs, appended as by AUTHOR.
Replies to unknown comments and empty replies are dropped."
  (mapcar (lambda (c)
            (let ((new (cl-loop for (id . body) in replies
                                when (and (equal id (plist-get c :id))
                                          (not (string-empty-p body)))
                                collect (list :author author :body body))))
              (if new
                  (plist-put (copy-sequence c) :replies
                             (append (plist-get c :replies) new))
                c)))
          comments))

;;;; Persistence

(defun agent-review--project-name (root)
  "Return the directory name used to store reviews rooted at ROOT."
  (or (file-name-nondirectory (directory-file-name (expand-file-name root)))
      "root"))

(defun agent-review--session-directory (s)
  "Return the storage directory of session S."
  (expand-file-name
   (concat (agent-review--project-name (agent-review-session-root s)) "/"
           (agent-review-session-id s) "/")
   agent-review-directory))

(defun agent-review--session-file (s)
  "Return the JSON file of session S."
  (expand-file-name "review.json" (agent-review--session-directory s)))

(defun agent-review--snapshot-json (snapshot)
  "Return the alist SNAPSHOT as a JSON array."
  (vconcat (mapcar (lambda (cell) (list :path (car cell) :content (cdr cell)))
                   snapshot)))

(defun agent-review--snapshot-read (data)
  "Return the snapshot alist stored as DATA."
  (mapcar (lambda (o) (cons (plist-get o :path) (plist-get o :content))) data))

(defun agent-review--comment-json (c)
  "Return comment C as a JSON-ready plist."
  (agent-review--obj
   :id (plist-get c :id) :file (plist-get c :file) :side (plist-get c :side)
   :start_line (plist-get c :start_line) :end_line (plist-get c :end_line)
   :anchor (plist-get c :anchor) :body (plist-get c :body)
   :author (plist-get c :author)
   :resolved (and (plist-get c :resolved) t)
   :drifted (and (plist-get c :drifted) t)
   :replies (and (plist-get c :replies)
                 (vconcat (mapcar (lambda (r)
                                    (list :author (plist-get r :author)
                                          :body (plist-get r :body)))
                                  (plist-get c :replies))))))

(defun agent-review--save (s)
  "Write session S to disk."
  (let ((file (agent-review--session-file s)))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file file
        (json-insert
         (agent-review--obj
          :id (agent-review-session-id s)
          :mode (symbol-name (agent-review-session-mode s))
          :root (agent-review-session-root s)
          :files (vconcat (agent-review-session-files s))
          :revision (agent-review--session-revision s)
          :round (agent-review-session-round s)
          :state (agent-review-session-state s)
          :comments (vconcat (mapcar #'agent-review--comment-json
                                     (agent-review-session-comments s)))
          :snapshot (agent-review--snapshot-json (agent-review-session-snapshot s))
          :prev_snapshot (agent-review--snapshot-json
                          (agent-review-session-prev-snapshot s))
          :prompt (agent-review-session-prompt s)
          :approved (and (agent-review-session-approved s) t)))))))

(defun agent-review--load-file (file)
  "Return the session stored in FILE."
  (let ((d (with-temp-buffer
             (let ((coding-system-for-read 'utf-8))
               (insert-file-contents file))
             (json-parse-buffer :object-type 'plist :array-type 'list
                                :null-object nil :false-object nil))))
    (agent-review--make-session
     :id (plist-get d :id)
     :mode (intern (plist-get d :mode))
     :root (plist-get d :root)
     :files (plist-get d :files)
     :revision (plist-get d :revision)
     :round (plist-get d :round)
     :state (plist-get d :state)
     :comments (plist-get d :comments)
     :snapshot (agent-review--snapshot-read (plist-get d :snapshot))
     :prev-snapshot (agent-review--snapshot-read (plist-get d :prev_snapshot))
     :prompt (plist-get d :prompt)
     :approved (plist-get d :approved))))

(defun agent-review--load (id &optional root)
  "Return session ID read from disk, optionally under project ROOT."
  (let ((file (if root
                  (expand-file-name
                   (concat (agent-review--project-name root) "/" id "/review.json")
                   agent-review-directory)
                (and (file-directory-p agent-review-directory)
                     (cl-find-if
                      (lambda (candidate)
                        (equal id (file-name-nondirectory
                                   (directory-file-name
                                    (file-name-directory candidate)))))
                      (directory-files-recursively agent-review-directory
                                                   "review\\.json\\'"))))))
    (when (and file (file-readable-p file))
      (agent-review--load-file file))))

(defun agent-review--register (s)
  "Make session S live and return it.
A session still waiting for the agent resumes watching for its replies."
  (puthash (agent-review-session-id s) s agent-review--sessions)
  (when (and (equal (agent-review-session-state s) "agent")
             (not (agent-review-session-approved s)))
    (agent-review--watch-replies s))
  s)

(defun agent-review--get (id)
  "Return session ID, loading it from disk when not live."
  (or (gethash id agent-review--sessions)
      (agent-review--register (or (agent-review--load id)
                                  (error "agent-review: no session %s" id)))))

(defun agent-review--all-sessions ()
  "Return every session found on disk or live."
  (when (file-directory-p agent-review-directory)
    (dolist (file (directory-files-recursively agent-review-directory
                                               "review\\.json\\'"))
      (ignore-errors
        (let ((s (agent-review--load-file file)))
          (unless (gethash (agent-review-session-id s) agent-review--sessions)
            (agent-review--register s))))))
  (hash-table-values agent-review--sessions))

;;;; Files and git

(defun agent-review--read-file (file)
  "Return the contents of regular FILE, or nil when it cannot be read."
  (when (and (file-regular-p file) (file-readable-p file))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents file))
      (buffer-string))))

(defun agent-review--abs (s file)
  "Return FILE of session S as an absolute file name."
  (expand-file-name file (agent-review-session-root s)))

(defun agent-review--display-name (s abs)
  "Return how session S names the absolute file ABS."
  (let ((rel (file-relative-name abs (agent-review-session-root s))))
    (if (string-prefix-p ".." rel) (abbreviate-file-name abs) rel)))

(defun agent-review--read-snapshot (s)
  "Return the current contents of the files of session S by display name."
  (when (eq (agent-review-session-mode s) 'files)
    (cl-loop for abs in (agent-review-session-files s)
             for content = (agent-review--read-file abs)
             when content
             collect (cons (agent-review--display-name s abs) content))))

(defun agent-review--git (root &rest args)
  "Run git with ARGS in ROOT and return its output."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory root))
           (coding-system-for-read 'utf-8)
           (status (apply #'process-file "git" nil '(t nil) nil args)))
      (unless (eql status 0)
        (error "agent-review: git %s failed" (string-join args " ")))
      (buffer-string))))

(defun agent-review--git-try (root &rest args)
  "Run git with ARGS in ROOT; return its trimmed output or nil on failure."
  (ignore-errors (string-trim (apply #'agent-review--git root args))))

(defun agent-review--git-toplevel (dir)
  "Return the top level of the git repository containing DIR."
  (or (agent-review--git-try dir "rev-parse" "--show-toplevel")
      (user-error "agent-review: %s is not inside a git repository" dir)))

(defun agent-review--git-commit (root revision)
  "Resolve REVISION to a commit hash in ROOT, or signal a user error."
  (or (agent-review--git-try root "rev-parse" "--verify"
                             (concat revision "^{commit}"))
      (user-error "agent-review: unknown commit %s" revision)))

(defun agent-review--git-commits (root)
  "Return local commits in ROOT as completion candidates."
  (mapcar (lambda (line)
            (let ((tab (string-match "\t" line)))
              (cons (substring line (1+ tab)) (substring line 0 tab))))
          (split-string
           (agent-review--git root "log" "--all" "--format=%H%x09%h %s")
           "\n" t)))

(defun agent-review--git-input (root input &rest args)
  "Run git with ARGS in ROOT, feeding INPUT to its standard input."
  (with-temp-buffer
    (insert input)
    (let* ((default-directory (file-name-as-directory root))
           (coding-system-for-read 'utf-8)
           (coding-system-for-write 'utf-8-unix)
           (status (apply #'call-process-region (point-min) (point-max)
                          "git" t '(t nil) nil args)))
      (unless (eql status 0)
        (error "agent-review: git %s failed: %s"
               (string-join args " ") (string-trim (buffer-string)))))))

(defun agent-review--git-hunk-patch (root file heading)
  "Return FILE's unstaged patch for the hunk whose header is HEADING."
  (let* ((diff (agent-review--git root "-c" "core.quotePath=false" "diff"
                                  "--no-color" "--no-ext-diff"
                                  "--src-prefix=a/" "--dst-prefix=b/"
                                  "--" file))
         (lines (split-string diff "\n"))
         (start (cl-position heading lines :test #'equal))
         (first-hunk (cl-position-if (lambda (line) (string-prefix-p "@@ " line)) lines))
         (end (and start
                   (cl-position-if (lambda (line) (string-prefix-p "@@ " line))
                                   lines :start (1+ start)))))
    (unless (and start first-hunk)
      (user-error "The hunk is no longer present; refresh the review"))
    (concat (string-join (append (cl-subseq lines 0 first-hunk)
                                 (cl-subseq lines start end))
                         "\n")
            "\n")))

(defun agent-review--git-entries (root &optional revision)
  "Return ROOT's working-tree or selected REVISION entries.
Without REVISION, show unstaged changes.  With REVISION,
show only that commit's changes against its first parent."
  (let ((diff (if revision
                  (agent-review--git
                   root "-c" "core.quotePath=false" "diff-tree"
                   "--root" "--first-parent" "-m" "-p" "--no-commit-id"
                   "--no-color" "--no-ext-diff"
                   "--src-prefix=a/" "--dst-prefix=b/" revision)
                (agent-review--git root "-c" "core.quotePath=false" "diff"
                                   "--no-color" "--no-ext-diff"
                                   "--src-prefix=a/" "--dst-prefix=b/")))
        (untracked (and (null revision)
                        (split-string
                         (agent-review--git root "ls-files" "--others"
                                            "--exclude-standard" "-z")
                         "\0" t))))
    (append
     (agent-review--parse-diff diff)
     (mapcan (lambda (f)
               (when-let* ((content (agent-review--read-file (expand-file-name f root))))
                 (cons (list :kind 'file :file f :note "untracked")
                       (cl-loop for text in (agent-review--lines content)
                                for n from 1
                                collect (list :kind 'add :file f :new n :text text)))))
             untracked))))

(defun agent-review--git-state (root)
  "Return a value that changes whenever the working tree under ROOT does.
It covers HEAD, the index, and the size and modification time of every
modified, deleted or untracked file, so a further edit to an already
modified file changes it too.  Return nil when git fails.
`git status' would be simpler but takes several times longer on large
repositories, and this runs on a timer."
  (ignore-errors
    (let ((stamp (lambda (file)
                   (when-let* ((attrs (file-attributes file)))
                     (list (file-attribute-modification-time attrs)
                           (file-attribute-size attrs))))))
      (list (agent-review--git-try root "rev-parse" "-q" "--verify" "HEAD")
            (funcall stamp (expand-file-name
                            (agent-review--git-try root "rev-parse" "--git-path" "index")
                            root))
            (mapcar (lambda (file)
                      (cons file (funcall stamp (expand-file-name file root))))
                    (split-string
                     (agent-review--git root "ls-files" "--modified" "--others"
                                        "--exclude-standard" "-z")
                     "\0" t))))))

(defun agent-review--diff-strings (old new)
  "Return a unified diff of OLD and NEW with every line as context."
  (let ((coding-system-for-write 'utf-8-unix)
        (coding-system-for-read 'utf-8))
    (let ((a (make-temp-file "agent-review-" nil nil old))
          (b (make-temp-file "agent-review-" nil nil new)))
      (unwind-protect
          (with-temp-buffer
            (call-process "diff" nil t nil "-U1000000" a b)
            (buffer-string))
        (delete-file a)
        (delete-file b)))))

;;;; Rendering

(defun agent-review--sync-font-lock-face (&optional object)
  "Copy the face property to font-lock-face throughout OBJECT.
OBJECT is a string, or the current buffer when nil.  Setting both keeps
the faces whether or not font-lock is on in the review buffer."
  (let ((pos (if object 0 (point-min)))
        (end (if object (length object) (point-max))))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'face object end)))
        (put-text-property pos next 'font-lock-face
                           (get-text-property pos 'face object) object)
        (setq pos next)))))

(defun agent-review--face (string face)
  "Return STRING shown in FACE."
  (propertize string 'face face 'font-lock-face face))

(defun agent-review--add-face (string face)
  "Return a copy of STRING with FACE under its existing faces."
  (let ((s (copy-sequence string)))
    (add-face-text-property 0 (length s) face t s)
    (agent-review--sync-font-lock-face s)
    s))

(defun agent-review--fontify (abs content)
  "Return the lines of CONTENT fontified in the major mode for ABS."
  (with-temp-buffer
    (insert content)
    (let ((mode (and (< (buffer-size) agent-review-fontify-max-size)
                     (assoc-default abs auto-mode-alist #'string-match))))
      (when (and mode (symbolp mode))
        (when (fboundp 'major-mode-remap)
          (setq mode (major-mode-remap mode)))
        (when (fboundp mode)
          ;; Mode hooks would start LSP clients and the like for a buffer
          ;; that only exists to borrow faces.
          (ignore-errors
            (delay-mode-hooks (funcall mode))
            (font-lock-ensure)))))
    (agent-review--sync-font-lock-face)
    (vconcat (split-string (buffer-string) "\n"))))

(defun agent-review--file-stamp (file)
  "Return a cheap change stamp for regular FILE, or nil when unavailable."
  (when-let* ((attrs (file-attributes file 'string)))
    (list (file-attribute-size attrs)
          (file-attribute-modification-time attrs))))

(defun agent-review--fontified-line (s file n)
  "Return line N of FILE of session S fontified, or nil when unavailable."
  (let* ((abs (agent-review--abs s file))
         (render-cache (or agent-review--fontify-render-cache
                           (setq agent-review--fontify-render-cache
                                 (make-hash-table :test #'equal))))
         (missing (make-symbol "missing"))
         (rendered (gethash abs render-cache missing))
         (lines
          (if (not (eq rendered missing))
              rendered
            (let* ((stamp (agent-review--file-stamp abs))
                   (cache (or agent-review--fontify-cache
                              (setq agent-review--fontify-cache
                                    (make-hash-table :test #'equal))))
                   (cached (gethash abs cache missing))
                   (value
                    (if (and (not (eq cached missing))
                             (equal stamp (car cached)))
                        (cdr cached)
                      (let ((fontified
                             (when-let* ((content (agent-review--read-file abs)))
                               (agent-review--fontify abs content))))
                        (puthash abs (cons stamp fontified) cache)
                        fontified))))
              (puthash abs value render-cache)
              value))))
    (and lines n (<= 1 n (length lines)) (aref lines (1- n)))))

(defun agent-review--glob-match-p (pattern file)
  "Return non-nil when shell wildcard PATTERN matches FILE."
  (string-match-p
   (or (gethash pattern agent-review--glob-regexp-cache)
       (puthash pattern (wildcard-to-regexp pattern)
                agent-review--glob-regexp-cache))
   file))

(defun agent-review--file-visible-p (file)
  "Return non-nil when project-relative FILE is selected for display."
  (if-let* ((override (assoc file agent-review--file-overrides)))
      (cdr override)
    (not (or (cl-some (lambda (regexp) (string-match-p regexp file))
                      agent-review-hidden-file-regexps)
             (cl-some (lambda (pattern) (agent-review--glob-match-p pattern file))
                      agent-review-hidden-file-globs)))))

(defun agent-review--visible-entries (entries)
  "Return ENTRIES belonging to files selected in the current review buffer."
  (cl-remove-if-not
   (lambda (entry)
     (if-let* ((file (plist-get entry :file)))
         (agent-review--file-visible-p file)
       t))
   entries))

(defun agent-review--review-files (s)
  "Return every file available for selection in session S."
  (delete-dups
   (delq nil (mapcar (lambda (entry) (plist-get entry :file))
                     (agent-review--cached-entries s)))))

(defun agent-review--file-entries (s abs)
  "Return the entries for the files review of ABS in session S."
  (let* ((name (agent-review--display-name s abs))
         (content (agent-review--read-file abs))
         (prev (cdr (assoc name (agent-review-session-prev-snapshot s)))))
    (cond
     ((null content)
      (list (list :kind 'file :file name :note "missing")))
     ((and agent-review--round-diff prev (not (equal prev content)))
      (cons (list :kind 'file :file name :note "changes since the previous review")
            (cl-remove-if (lambda (e) (memq (plist-get e :kind) '(file hunk)))
                          (agent-review--parse-diff
                           (agent-review--diff-strings prev content) name))))
     (t
      (cons (list :kind 'file :file name)
            (cl-loop for text in (agent-review--lines content)
                     for n from 1
                     collect (list :kind 'ctx :file name :old n :new n :text text)))))))

(defun agent-review--entries (s)
  "Return the entries rendered for session S."
  (if (eq (agent-review-session-mode s) 'git)
      (agent-review--git-entries (agent-review-session-root s)
                                 (agent-review--session-revision s))
    (mapcan (lambda (abs) (agent-review--file-entries s abs))
            (agent-review-session-files s))))

(defun agent-review--cached-entries (s)
  "Return S's entries, reusing them until content is invalidated."
  (unless agent-review--entries-cache-valid
    (setq agent-review--entries-cache (agent-review--entries s)
          agent-review--entries-cache-valid t))
  agent-review--entries-cache)

(defun agent-review--invalidate-content-cache (&optional clear-fontify)
  "Invalidate cached entries, and fontification when CLEAR-FONTIFY is non-nil."
  (setq agent-review--entries-cache nil
        agent-review--entries-cache-valid nil)
  (when (and clear-fontify (hash-table-p agent-review--fontify-cache))
    (clrhash agent-review--fontify-cache)))

(defun agent-review--loc-key (file side line)
  "Return the key under which comments on LINE of FILE's SIDE are indexed."
  (list file (or side "new") line))

(defun agent-review--comment-block (c)
  "Return the text showing comment C."
  (let* ((indent "        ")
         (file (plist-get c :file))
         (start (plist-get c :start_line))
         (end (plist-get c :end_line))
         (resolved (plist-get c :resolved))
         (range (cond ((null file) "general")
                      ((= start end) (format "L%d" start))
                      (t (format "L%d-%d" start end))))
         (text
          (concat
           (format "%s┌─ %s  %s%s  %s%s%s\n" indent
                   (plist-get c :author) range
                   (if (equal (plist-get c :side) "old") " (old)" "")
                   (plist-get c :id)
                   (if resolved "  [resolved]" "")
                   (if (plist-get c :drifted) "  [drifted]" ""))
           (mapconcat (lambda (l) (concat indent "│ " l "\n"))
                      (split-string (plist-get c :body) "\n") "")
           (mapconcat
            (lambda (r)
              (let ((lines (split-string (plist-get r :body) "\n")))
                (concat indent "│ ↳ " (plist-get r :author) ": " (car lines) "\n"
                        (mapconcat (lambda (l) (concat indent "│     " l "\n"))
                                   (cdr lines) ""))))
            (plist-get c :replies) "")
           indent "└─\n"))
         (face (if resolved 'agent-review-resolved 'agent-review-comment)))
    (propertize text 'face face 'font-lock-face face
                'agent-review-comment (plist-get c :id))))

(defun agent-review--insert-line (s e diff-p hunk)
  "Insert line E of session S; DIFF-P selects the gutter and HUNK identifies it."
  (let* ((kind (plist-get e :kind))
         (file (plist-get e :file))
         (side (if (eq kind 'del) "old" "new"))
         (line (plist-get e (if (eq kind 'del) :old :new)))
         (sign (pcase kind ('add "+") ('del "-") (_ " ")))
         (gutter (if diff-p
                     (format "%5s %5s %s│ "
                             (or (plist-get e :old) "") (or (plist-get e :new) "") sign)
                   (format "%5d │ " line)))
         (body (concat (or (and (not (agent-review--session-revision s))
                                (not (eq kind 'del))
                                (agent-review--fontified-line s file line))
                           (plist-get e :text))
                       "\n"))
         (body (pcase kind
                 ('add (agent-review--add-face body 'agent-review-added))
                 ('del (agent-review--add-face body 'agent-review-removed))
                 (_ body))))
    (insert (propertize (concat (agent-review--face gutter 'agent-review-line-number) body)
                        'agent-review-line
                        (list :file file :side side :line line :text (plist-get e :text))
                        'agent-review-hunk hunk))))

(defun agent-review--hunk-heading (e)
  "Return the heading for hunk entry E with its project-relative file name."
  (concat (plist-get e :text)
          (when-let* ((file (plist-get e :file)))
            (format " : ./%s" file))))

(defun agent-review--point-anchor ()
  "Return what identifies the position of point across a re-render."
  (list (get-text-property (point) 'agent-review-comment)
        (get-text-property (point) 'agent-review-line)
        (current-column)
        (line-number-at-pos)))

(defun agent-review--find-prop (prop value)
  "Return the start of the first text with PROP equal to VALUE, or nil."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((m (text-property-search-forward prop value t)))
      (prop-match-beginning m))))

(defun agent-review--restore-point (anchor)
  "Move point back to ANCHOR from `agent-review--point-anchor'."
  (pcase-let ((`(,id ,loc ,col ,line) anchor))
    (let ((pos (or (and id (agent-review--find-prop 'agent-review-comment id))
                   (and loc (agent-review--find-prop 'agent-review-line loc)))))
      (if pos
          (progn (goto-char pos) (move-to-column col))
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun agent-review--render ()
  "Redraw the current review buffer, keeping point where it was."
  (let* ((s agent-review--session)
         (inhibit-read-only t)
         (anchor (agent-review--point-anchor))
         (win (get-buffer-window (current-buffer)))
         (start-line (and win (line-number-at-pos (window-start win))))
         (comments (agent-review--unresolved (agent-review-session-comments s)))
         (index (make-hash-table :test #'equal))
         (placed (make-hash-table :test #'equal))
         (diff-p (or (eq (agent-review-session-mode s) 'git) agent-review--round-diff))
         (agent-review--fontify-render-cache (make-hash-table :test #'equal)))
    (dolist (c (reverse comments))
      (when (plist-get c :file)
        (push c (gethash (agent-review--loc-key (plist-get c :file) (plist-get c :side)
                                                (plist-get c :end_line))
                         index))))
    (erase-buffer)
    (insert (agent-review--face
             (format "%s review of %s%s  (session %s)\n"
                     (if (eq (agent-review-session-mode s) 'git) "Git" "File")
                     (abbreviate-file-name (agent-review-session-root s))
                     (if-let* ((revision (agent-review--session-revision s)))
                         (format " at %.12s" revision)
                       "")
                     (agent-review-session-id s))
             'bold))
    (let ((general (cl-remove-if (lambda (c) (plist-get c :file)) comments)))
      (when general
        (insert "\n" (agent-review--face "General comments\n" 'agent-review-file-header))
        (dolist (c general) (insert (agent-review--comment-block c)))))
    (let (hunk)
      (dolist (e (agent-review--visible-entries (agent-review--cached-entries s)))
        (pcase (plist-get e :kind)
          ('file
           (setq hunk nil)
           (insert "\n" (propertize
                         (agent-review--face
                          (concat (plist-get e :file)
                                  (if (plist-get e :note)
                                      (format "  (%s)" (plist-get e :note)) "")
                                  "\n")
                          'agent-review-file-header)
                         'agent-review-file (plist-get e :file))))
          ('hunk
           (setq hunk e)
           (insert (propertize
                    (agent-review--face (concat (agent-review--hunk-heading e) "\n")
                                        'agent-review-hunk)
                    'agent-review-hunk e)))
          (kind
           (agent-review--insert-line s e diff-p hunk)
           (dolist (c (gethash (agent-review--loc-key
                                (plist-get e :file) (if (eq kind 'del) "old" "new")
                                (plist-get e (if (eq kind 'del) :old :new)))
                               index))
             (unless (gethash (plist-get c :id) placed)
               (puthash (plist-get c :id) t placed)
               (insert (agent-review--comment-block c))))))))
    (let ((unplaced (cl-remove-if (lambda (c) (or (null (plist-get c :file))
                                                  (gethash (plist-get c :id) placed)))
                                  comments)))
      (when unplaced
        (insert "\n" (agent-review--face "Comments on lines not shown\n"
                                         'agent-review-file-header))
        (dolist (c unplaced)
          (insert (agent-review--face (format "    %s\n" (plist-get c :file)) 'bold)
                  (agent-review--comment-block c)))))
    (agent-review--restore-point anchor)
    (when win
      (set-window-start win (save-excursion (goto-char (point-min))
                                            (forward-line (1- start-line))
                                            (point)))
      (set-window-point win (point)))))

(defun agent-review--header-line ()
  "Return the header line of the current review buffer."
  (let ((s agent-review--session))
    (format " %s · %d open │ c comment  C general  r reply  R resolve  e edit  D delete  f files%s%s  N next review  C-c C-c finish"
            (cond
             ((equal (agent-review-session-state s) "reviewing") "reviewing")
             ((agent-review-session-approved s) "approved")
             (t "waiting for the agent's replies"))
            (length (agent-review--unresolved (agent-review-session-comments s)))
            (if (eq (agent-review-session-mode s) 'files) "  d review diff" "")
            (if (eq (agent-review-session-mode s) 'git)
                "  s stage  S select commit" ""))))

;;;; Buffers and file watching

(defun agent-review--schedule-refresh (buf)
  "Redraw BUF shortly, coalescing bursts of file events."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (timerp agent-review--refresh-timer)
        (cancel-timer agent-review--refresh-timer))
      (setq agent-review--refresh-timer
            (run-with-timer 0.3 nil
                            (lambda ()
                              (when (buffer-live-p buf)
                                (with-current-buffer buf
                                  (setq agent-review--refresh-timer nil)
                                  (agent-review--refresh-content)))))))))

(defun agent-review--poll-git (buf)
  "Redraw git review BUF if its working tree changed since the last check.
Hidden buffers are skipped; the first check after one is shown again
picks up everything that changed meanwhile."
  (when (and (buffer-live-p buf) (get-buffer-window buf t))
    (with-current-buffer buf
      (let ((state (agent-review--git-state
                    (agent-review-session-root agent-review--session))))
        (unless (equal state agent-review--git-state)
          (setq agent-review--git-state state)
          (agent-review--schedule-refresh buf))))))

(defun agent-review--watch ()
  "Redraw the current review whenever relevant working-tree files change."
  (let* ((s agent-review--session)
         (buf (current-buffer))
         (files (agent-review-session-files s))
         (truenames (mapcar #'file-truename files)))
    (if (eq (agent-review-session-mode s) 'git)
        ;; A git review can concern any file in the repository, and watching
        ;; every directory exhausts the file descriptors kqueue needs, so ask
        ;; git what changed instead.
        (unless (agent-review--session-revision s)
          (setq agent-review--git-state
                (agent-review--git-state (agent-review-session-root s)))
          (setq agent-review--poll-timer
                (run-with-timer agent-review-git-poll-interval
                                agent-review-git-poll-interval
                                #'agent-review--poll-git buf)))
      ;; Agents often replace a file by renaming over it, which ends a watch
      ;; on the file itself, so watch its directory.
      (dolist (dir (delete-dups (mapcar #'file-name-directory files)))
        (when (file-directory-p dir)
          (ignore-errors
            (push (file-notify-add-watch
                   dir '(change)
                   (lambda (event)
                     (when (cl-some (lambda (f)
                                      (and (stringp f)
                                           (member (file-truename f) truenames)))
                                    (cddr event))
                       (agent-review--schedule-refresh buf))))
                  agent-review--watches)))))))

(defun agent-review--unwatch ()
  "Stop the file watches, polling and pending refresh of the current buffer."
  (mapc #'file-notify-rm-watch agent-review--watches)
  (setq agent-review--watches nil)
  (when (timerp agent-review--poll-timer)
    (cancel-timer agent-review--poll-timer))
  (setq agent-review--poll-timer nil)
  (when (timerp agent-review--refresh-timer)
    (cancel-timer agent-review--refresh-timer))
  (setq agent-review--refresh-timer nil))

(defun agent-review--apply-dir-locals ()
  "Apply directory-local variables for the current review's project."
  (setq default-directory (agent-review-session-root agent-review--session))
  (hack-dir-local-variables-non-file-buffer))

(defun agent-review--ensure-buffer (s)
  "Return the review buffer of session S, creating it when needed."
  (let ((buf (agent-review-session-buffer s)))
    (unless (buffer-live-p buf)
      (setq buf (generate-new-buffer
                 (format "*agent-review: %s*"
                         (if (eq (agent-review-session-mode s) 'git)
                             (concat "git "
                                     (file-name-nondirectory
                                      (directory-file-name (agent-review-session-root s)))
                                     (if-let* ((revision (agent-review--session-revision s)))
                                         (format "@%.12s" revision)
                                       ""))
                           (mapconcat #'file-name-nondirectory
                                      (agent-review-session-files s) " ")))))
      (with-current-buffer buf
        (agent-review-mode)
        (setq agent-review--session s)
        (agent-review--apply-dir-locals)
        (agent-review--watch)
        (agent-review--render))
      (setf (agent-review-session-buffer s) buf))
    buf))

(defun agent-review--changed (s &optional content-changed)
  "Save session S and redraw its buffer.
Invalidate content caches when CONTENT-CHANGED is non-nil."
  (agent-review--save s)
  (let ((buf (agent-review-session-buffer s)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when content-changed
          (agent-review--invalidate-content-cache))
        (agent-review--render)))))

(defun agent-review--display (s)
  "Show the review buffer of session S."
  (pop-to-buffer (agent-review--ensure-buffer s)))

(defun agent-review--open (mode root files &optional revision)
  "Return the session for MODE over ROOT, FILES and REVISION."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (revision (and revision (agent-review--git-commit root revision)))
         (branch (and (eq mode 'git) (null revision)
                      (agent-review--git-try root "rev-parse" "--abbrev-ref" "HEAD")))
         (id (agent-review--session-id mode root files branch revision)))
    (or (gethash id agent-review--sessions)
        (when-let* ((loaded (agent-review--load id root)))
          (agent-review--register loaded))
        (let ((new (agent-review--make-session :id id :mode mode :root root
                                               :files files :revision revision)))
          (setf (agent-review-session-snapshot new) (agent-review--read-snapshot new))
          (agent-review--save new)
          (agent-review--register new)))))

(defun agent-review--reanchor-comment (s c)
  "Return comment C of session S moved to its anchor in the current file."
  (let ((content (and (plist-get c :file)
                      (not (equal (plist-get c :side) "old"))
                      (agent-review--read-file (agent-review--abs s (plist-get c :file))))))
    (if content
        (agent-review--reanchor c (vconcat (agent-review--lines content)))
      c)))

(defun agent-review--reanchor-comments (s)
  "Move comments of S to their anchors when the current files permit it.
Return non-nil when any stored comment changed."
  (let* ((before (agent-review-session-comments s))
         (after (mapcar (lambda (c) (agent-review--reanchor-comment s c)) before)))
    (unless (equal before after)
      (setf (agent-review-session-comments s) after)
      t)))

(defun agent-review--refresh-content ()
  "Refresh changed content and keep comments attached when possible."
  (let ((s (agent-review--current)))
    (agent-review--invalidate-content-cache)
    (when (and (not (agent-review--session-revision s))
               (agent-review--reanchor-comments s))
      (agent-review--save s))
    (agent-review--render)))

(defun agent-review--start-round (s)
  "Start the next round of session S."
  (agent-review--unwatch-replies s)
  (cl-incf (agent-review-session-round s))
  (setf (agent-review-session-prev-snapshot s) (agent-review-session-snapshot s)
        (agent-review-session-snapshot s) (agent-review--read-snapshot s)
        (agent-review-session-comments s)
        (mapcar (lambda (c) (agent-review--reanchor-comment s c))
                (agent-review-session-comments s))
        (agent-review-session-state s) "reviewing"
        (agent-review-session-prompt s) nil
        (agent-review-session-approved s) nil)
  (agent-review--changed s t)
  (message "agent-review: review is ready"))

;;;; Agent replies

(defun agent-review--reply-file (s)
  "Return the file the agent writes its replies to session S in."
  (expand-file-name "replies.json" (agent-review--session-directory s)))

(defun agent-review--read-replies (s)
  "Add the agent's replies in the reply file of session S to its comments.
Return non-nil when the file was complete and has been consumed.  A file
that does not parse yet is left alone, as the agent may still be writing it."
  (let* ((file (agent-review--reply-file s))
         (replies (and (file-readable-p file)
                       (condition-case nil
                           (agent-review--parse-replies
                            (with-temp-buffer
                              (let ((coding-system-for-read 'utf-8))
                                (insert-file-contents file))
                              (buffer-string)))
                         (error 'invalid)))))
    (when (and replies (not (eq replies 'invalid)))
      (setf (agent-review-session-comments s)
            (agent-review--add-replies (agent-review-session-comments s)
                                       replies "agent")))
    (when (and (file-readable-p file) (not (eq replies 'invalid)))
      (delete-file file)
      t)))

(defun agent-review--replies-arrived (id)
  "Start the next round of session ID if the agent has replied."
  (let ((s (gethash id agent-review--sessions)))
    (when (and s (equal (agent-review-session-state s) "agent")
               (agent-review--read-replies s))
      (agent-review--start-round s)
      (when (buffer-live-p (agent-review-session-buffer s))
        (display-buffer (agent-review-session-buffer s))))))

(defun agent-review--watch-replies (s)
  "Start the next round of session S once the agent writes its replies."
  (agent-review--unwatch-replies s)
  (let* ((id (agent-review-session-id s))
         (file (agent-review--reply-file s))
         (dir (file-name-directory file))
         (cell (list nil)))
    (make-directory dir t)
    (setcar cell
            (file-notify-add-watch
             dir '(change)
             (lambda (event)
               (when (cl-some (lambda (f)
                                (and (stringp f)
                                     (equal (expand-file-name f) file)))
                              (cddr event))
                 ;; Wait for a burst of writes to settle before reading.
                 (when (timerp (cdr cell))
                   (cancel-timer (cdr cell)))
                 (setcdr cell (run-with-timer
                               0.5 nil #'agent-review--replies-arrived id))))))
    (puthash id cell agent-review--reply-watches)
    ;; The agent may have replied while nothing was watching.
    (when (file-exists-p file)
      (agent-review--replies-arrived id))))

(defun agent-review--unwatch-replies (s)
  "Stop waiting for the agent's replies to session S."
  (let ((cell (gethash (agent-review-session-id s) agent-review--reply-watches)))
    (when cell
      (when (car cell) (file-notify-rm-watch (car cell)))
      (when (timerp (cdr cell)) (cancel-timer (cdr cell)))
      (remhash (agent-review-session-id s) agent-review--reply-watches))))

(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-cwd "agent-shell-project")
(declare-function agent-shell-insert "agent-shell")

(defun agent-review--agent-shell (root)
  "Return the agent-shell buffer to send the review of ROOT to, or nil.
Shells started in or above ROOT come first; with several, ask."
  (when (featurep 'agent-shell)
    (let* ((all (agent-shell-buffers))
           (here (cl-remove-if-not
                  (lambda (b)
                    (let ((cwd (with-current-buffer b (agent-shell-cwd))))
                      (and cwd (file-in-directory-p root cwd))))
                  all))
           (candidates (or here all)))
      (if (cdr candidates)
          (get-buffer (completing-read "Send the review to: "
                                       (mapcar #'buffer-name candidates) nil t))
        (car candidates)))))

;;;; Comment editing

(defvar-local agent-review--edit-callback nil
  "Function called with the body when the edit buffer is committed.")

(defvar-keymap agent-review-edit-mode-map
  "C-c C-c" #'agent-review-edit-commit
  "C-c C-k" #'agent-review-edit-abort)

(define-derived-mode agent-review-edit-mode text-mode "Agent-Review-Edit"
  "Major mode for writing an agent-review comment or reply.")

(defun agent-review--read-body (title initial callback)
  "Edit a comment titled TITLE starting from INITIAL, then call CALLBACK."
  (let ((buf (get-buffer-create "*agent-review-edit*")))
    (with-current-buffer buf
      (erase-buffer)
      (agent-review-edit-mode)
      (when initial (insert initial))
      (setq agent-review--edit-callback callback)
      (setq header-line-format
            (string-replace "%" "%%" (format " %s — C-c C-c save, C-c C-k cancel" title))))
    (pop-to-buffer buf '((display-buffer-below-selected) (window-height . 10)))))

(defun agent-review-edit-commit ()
  "Save the comment being edited."
  (interactive)
  (let ((body (string-trim (buffer-string)))
        (callback agent-review--edit-callback))
    (when (string-empty-p body)
      (user-error "Empty comment"))
    (quit-window t)
    (funcall callback body)))

(defun agent-review-edit-abort ()
  "Discard the comment being edited."
  (interactive)
  (quit-window t))

(defun agent-review--current ()
  "Return the session of the current buffer."
  (or agent-review--session (user-error "Not in an agent-review buffer")))

(defun agent-review--comment-id-at-point ()
  "Return the id of the comment at point."
  (or (get-text-property (point) 'agent-review-comment)
      (user-error "No comment at point")))

(defun agent-review--find-comment (s id)
  "Return comment ID of session S."
  (cl-find id (agent-review-session-comments s)
           :key (lambda (c) (plist-get c :id)) :test #'equal))

(defun agent-review--update (s id fn)
  "Replace comment ID of session S with FN applied to a copy of it.
FN returning nil deletes the comment."
  (setf (agent-review-session-comments s)
        (delq nil (mapcar (lambda (c)
                            (if (equal (plist-get c :id) id)
                                (funcall fn (copy-sequence c))
                              c))
                          (agent-review-session-comments s))))
  (agent-review--changed s))

(defun agent-review--new-id (s)
  "Return a comment id unused in session S."
  (let (id)
    (while (or (null id) (agent-review--find-comment s id))
      (setq id (format "c_%08x" (random (ash 1 32)))))
    id))

(defun agent-review--add-comment (s &rest props)
  "Add a comment with PROPS by the reviewer to session S."
  (setf (agent-review-session-comments s)
        (append (agent-review-session-comments s)
                (list (append (list :id (agent-review--new-id s)
                                    :author agent-review-author)
                              props))))
  (agent-review--changed s))

(defun agent-review--anchor-text (file side start end)
  "Return the text of lines START to END of FILE's SIDE shown in the buffer."
  (let (lines)
    (save-excursion
      (goto-char (point-min))
      (let (m)
        (while (setq m (text-property-search-forward 'agent-review-line nil nil))
          (let ((loc (prop-match-value m)))
            (when (and (equal (plist-get loc :file) file)
                       (equal (plist-get loc :side) side)
                       (<= start (plist-get loc :line) end))
              (push (cons (plist-get loc :line) (plist-get loc :text)) lines))))))
    (mapconcat #'cdr (sort (seq-uniq lines (lambda (a b) (= (car a) (car b))))
                           (lambda (a b) (< (car a) (car b))))
               "\n")))

(defun agent-review--region-locs ()
  "Return the line locations at both ends of the region, or twice at point."
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (end (if (and (> end beg) (save-excursion (goto-char end) (bolp)))
                      (1- end)
                    end)))
        (list (get-text-property beg 'agent-review-line)
              (get-text-property end 'agent-review-line)))
    (let ((loc (get-text-property (point) 'agent-review-line)))
      (list loc loc))))

;;;; Commands

(defun agent-review-comment ()
  "Comment on the line at point, or on the lines of the region."
  (interactive)
  (pcase-let ((s (agent-review--current))
              (`(,a ,b) (agent-review--region-locs)))
    (unless (and a b)
      (user-error "Not on a line of a file"))
    (unless (and (equal (plist-get a :file) (plist-get b :file))
                 (equal (plist-get a :side) (plist-get b :side)))
      (user-error "A comment must cover lines of one file on one side of the diff"))
    (let* ((file (plist-get a :file))
           (side (plist-get a :side))
           (start (min (plist-get a :line) (plist-get b :line)))
           (end (max (plist-get a :line) (plist-get b :line)))
           (anchor (agent-review--anchor-text file side start end)))
      (deactivate-mark)
      (agent-review--read-body
       (if (= start end)
           (format "Comment on %s:%d" file start)
         (format "Comment on %s:%d-%d" file start end))
       nil
       (lambda (body)
         (agent-review--add-comment s :file file :side side :start_line start
                                    :end_line end :anchor anchor :body body))))))

(defun agent-review-comment-general ()
  "Add a comment on the review as a whole."
  (interactive)
  (let ((s (agent-review--current)))
    (agent-review--read-body "General comment" nil
                             (lambda (body) (agent-review--add-comment s :body body)))))

(defun agent-review-edit ()
  "Edit the body of the comment at point."
  (interactive)
  (let* ((s (agent-review--current))
         (id (agent-review--comment-id-at-point)))
    (agent-review--read-body (format "Edit %s" id)
                             (plist-get (agent-review--find-comment s id) :body)
                             (lambda (body)
                               (agent-review--update s id (lambda (c) (plist-put c :body body)))))))

(defun agent-review-reply ()
  "Reply to the comment at point."
  (interactive)
  (let* ((s (agent-review--current))
         (id (agent-review--comment-id-at-point)))
    (agent-review--read-body
     (format "Reply to %s" id) nil
     (lambda (body)
       (agent-review--update
        s id (lambda (c)
               (plist-put c :replies
                          (append (plist-get c :replies)
                                  (list (list :author agent-review-author :body body))))))))))

(defun agent-review-delete ()
  "Delete the comment at point."
  (interactive)
  (let* ((s (agent-review--current))
         (id (agent-review--comment-id-at-point)))
    (when (yes-or-no-p (format "Delete comment %s? " id))
      (agent-review--update s id #'ignore))))

(defun agent-review-toggle-resolved ()
  "Mark the comment at point resolved, or open it again."
  (interactive)
  (let ((s (agent-review--current)))
    (agent-review--update s (agent-review--comment-id-at-point)
                          (lambda (c) (plist-put c :resolved (not (plist-get c :resolved)))))))

(defun agent-review-next-comment ()
  "Move to the next comment."
  (interactive)
  (let ((m (save-excursion (text-property-search-forward 'agent-review-comment nil nil t))))
    (if m (goto-char (prop-match-beginning m)) (user-error "No next comment"))))

(defun agent-review-previous-comment ()
  "Move to the previous comment."
  (interactive)
  (let ((m (save-excursion (text-property-search-backward 'agent-review-comment nil nil t))))
    (if m (goto-char (prop-match-beginning m)) (user-error "No previous comment"))))

(defun agent-review-select-files ()
  "Select the files displayed in the current review buffer.
The selection overrides the configured regexps and glob patterns until this
buffer is closed."
  (interactive)
  (let* ((s (agent-review--current))
         (files (agent-review--review-files s)))
    (unless files
      (user-error "There are no files to select"))
    (let* ((visible (cl-remove-if-not #'agent-review--file-visible-p files))
           (review-buffer (current-buffer))
           (completion-extra-properties
            `(:annotation-function
              ,(lambda (file)
                 (with-current-buffer review-buffer
                   (if (agent-review--file-visible-p file) " [shown]" " [hidden]")))))
           (selected (completing-read-multiple
                      "Files to show: " files nil t nil nil visible)))
      (setq agent-review--file-overrides nil)
      (dolist (file files)
        (setf (alist-get file agent-review--file-overrides nil nil #'equal)
              (and (member file selected) t)))
      (agent-review--render)
      (message "Showing %d of %d files" (length selected) (length files)))))

(defun agent-review-toggle-round-diff ()
  "Toggle showing what changed in the files since the previous round."
  (interactive)
  (let ((s (agent-review--current)))
    (unless (eq (agent-review-session-mode s) 'files)
      (user-error "A git review already shows a diff"))
    (unless (agent-review-session-prev-snapshot s)
      (user-error "There is no previous review"))
    (setq agent-review--round-diff (not agent-review--round-diff))
    (agent-review--invalidate-content-cache)
    (agent-review--render)))

(defun agent-review-visit ()
  "Visit the file line at point."
  (interactive)
  (let* ((s (agent-review--current))
         (loc (or (get-text-property (point) 'agent-review-line)
                  (user-error "Not on a line of a file"))))
    (find-file-other-window (agent-review--abs s (plist-get loc :file)))
    (goto-char (point-min))
    (forward-line (1- (plist-get loc :line)))))

(defun agent-review-finish ()
  "Finish the round and send the unresolved comments to an agent-shell.
Without an agent-shell, the instructions go to the kill ring for pasting
into the agent instead.  The next round starts when the agent replies."
  (interactive)
  (let* ((s (agent-review--current))
         (comments (agent-review-session-comments s))
         (approved (null (agent-review--unresolved comments)))
         (prompt (agent-review--finish-prompt comments (agent-review--reply-file s))))
    (unless (equal (agent-review-session-state s) "reviewing")
      (user-error "Review is already finished"))
    (setf (agent-review-session-prompt s) prompt
          (agent-review-session-approved s) approved
          (agent-review-session-state s) "agent")
    (agent-review--changed s)
    (unless approved
      (agent-review--watch-replies s))
    (let ((shell (agent-review--agent-shell (agent-review-session-root s))))
      (if shell
          (agent-shell-insert :text prompt :submit t :shell-buffer shell)
        (kill-new prompt))
      (message "agent-review: %s; %s"
               (if approved "approved" "finished with open comments")
               (if shell
                   (format "sent to %s" (buffer-name shell))
                 "no agent-shell is running, so the prompt is in the kill ring")))))

(defun agent-review-new-round ()
  "Start the next round without waiting for the agent's replies.
Replies the agent has already written are still taken in."
  (interactive)
  (let ((s (agent-review--current)))
    (when (equal (agent-review-session-state s) "reviewing")
      (user-error "Review is still being reviewed"))
    (agent-review--read-replies s)
    (agent-review--start-round s)))

(defun agent-review--describe (s)
  "Return a one-line description of session S."
  (format "%s  %s  %s  (%d open)"
          (agent-review-session-id s)
          (if (eq (agent-review-session-mode s) 'git)
              (concat "git " (abbreviate-file-name (agent-review-session-root s))
                      (if-let* ((revision (agent-review--session-revision s)))
                          (format " at %.12s" revision)
                        ""))
            (mapconcat (lambda (f) (agent-review--display-name s f))
                       (agent-review-session-files s) ", "))
          (agent-review-session-state s)
          (length (agent-review--unresolved (agent-review-session-comments s)))))

(defun agent-review-select-commit ()
  "Select a local commit, or return to the unstaged changes review."
  (interactive)
  (let ((s (agent-review--current)))
    (unless (eq (agent-review-session-mode s) 'git)
      (user-error "Commit selection is available only in a Git review"))
    (let* ((root (agent-review-session-root s))
           (working "Working tree (unstaged changes)")
           (table (cons (cons working nil) (agent-review--git-commits root)))
           (revision (agent-review--session-revision s))
           (current (or (car (rassoc revision table)) working))
           (choice (completing-read "Review commit: " table nil t nil nil current)))
      (agent-review--display
       (agent-review--open 'git root nil (cdr (assoc choice table)))))))

(defun agent-review-stage ()
  "Stage the hunk at point, or the whole file when no hunk is available."
  (interactive)
  (let ((s (agent-review--current)))
    (unless (eq (agent-review-session-mode s) 'git)
      (user-error "Staging is available only in a Git review"))
    (when (agent-review--session-revision s)
      (user-error "A historical commit cannot be staged"))
    (let* ((pos (max (point-min) (1- (point))))
           (hunk (or (get-text-property (point) 'agent-review-hunk)
                     (get-text-property pos 'agent-review-hunk)))
           (loc (or (get-text-property (point) 'agent-review-line)
                    (get-text-property pos 'agent-review-line)))
           (file (or (plist-get hunk :file)
                     (plist-get loc :file)
                     (get-text-property (point) 'agent-review-file)
                     (get-text-property pos 'agent-review-file)))
           (root (agent-review-session-root s)))
      (unless file (user-error "No file or hunk at point"))
      (if hunk
          (agent-review--git-input
           root (agent-review--git-hunk-patch root file (plist-get hunk :text))
           "apply" "--cached" "-")
        (agent-review--git root "add" "--" file))
      (agent-review--invalidate-content-cache)
      (agent-review--render)
      (message "Staged %s%s" file (if hunk " hunk" "")))))

(defvar-keymap agent-review-mode-map
  :parent special-mode-map
  "c" #'agent-review-comment
  "C" #'agent-review-comment-general
  "e" #'agent-review-edit
  "D" #'agent-review-delete
  "r" #'agent-review-reply
  "R" #'agent-review-toggle-resolved
  "n" #'agent-review-next-comment
  "p" #'agent-review-previous-comment
  "f" #'agent-review-select-files
  "s" #'agent-review-stage
  "S" #'agent-review-select-commit
  "d" #'agent-review-toggle-round-diff
  "N" #'agent-review-new-round
  "RET" #'agent-review-visit
  "C-c C-c" #'agent-review-finish)

;; `defvar-keymap' keeps an existing map when this file is reloaded.
(keymap-set agent-review-mode-map "f" #'agent-review-select-files)
(keymap-set agent-review-mode-map "s" #'agent-review-stage)
(keymap-set agent-review-mode-map "S" #'agent-review-select-commit)

(define-derived-mode agent-review-mode special-mode "Agent-Review"
  "Major mode for reviewing agent output.

\\{agent-review-mode-map}"
  (setq-local header-line-format '(:eval (agent-review--header-line)))
  (setq-local agent-review--fontify-cache (make-hash-table :test #'equal))
  (setq-local revert-buffer-function
              (lambda (&rest _)
                (agent-review--apply-dir-locals)
                (agent-review--invalidate-content-cache t)
                (agent-review--render)))
  (add-hook 'kill-buffer-hook #'agent-review--unwatch nil t))

(defun agent-review--default-root (file)
  "Return the root a files review of FILE names its files from."
  (let ((default-directory (file-name-directory file)))
    (if-let* ((project (project-current)))
        (expand-file-name (project-root project))
      default-directory)))

;;;###autoload
(defun agent-review (&optional files)
  "Review the changes of the current git repository, or FILES.
Like the agent-review CLI, no files means the git changes.
Interactively, a prefix argument reads a file to review instead."
  (interactive
   (and current-prefix-arg
        (list (list (read-file-name "Review file: " nil buffer-file-name t
                                    (and buffer-file-name
                                         (file-name-nondirectory buffer-file-name)))))))
  (if files
      (agent-review-files files)
    (agent-review-git)))

;;;###autoload
(defun agent-review-files (files)
  "Review FILES; interactively, the file of the current buffer or one read."
  (interactive
   (list (list (read-file-name "Review file: " nil buffer-file-name t
                               (and buffer-file-name
                                    (file-name-nondirectory buffer-file-name))))))
  (let ((abs (mapcar #'expand-file-name files)))
    (agent-review--display
     (agent-review--open 'files (agent-review--default-root (car abs)) abs))))

;;;###autoload
(defun agent-review-git ()
  "Review the changes of the current git repository."
  (interactive)
  (agent-review--display
   (agent-review--open 'git (agent-review--git-toplevel default-directory) nil)))

;;;###autoload
(defun agent-review-resume ()
  "Reopen a saved review."
  (interactive)
  (let ((table (mapcar (lambda (s) (cons (agent-review--describe s) s))
                       (agent-review--all-sessions))))
    (unless table (user-error "No reviews"))
    (agent-review--display
     (cdr (assoc (completing-read "Resume review: " table nil t) table)))))

(provide 'agent-review)
;;; agent-review.el ends here
