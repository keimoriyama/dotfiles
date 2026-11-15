;;; agent-review.el --- Review agent plans and diffs in Emacs buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; A review loop for coding agents that stays inside Emacs, modelled on
;; crit (https://github.com/tomasz-tomczyk/crit).
;;
;; The reviewer opens a review with M-x agent-review, comments on lines or
;; ranges and finishes the round with C-c C-c, which sends the unresolved
;; comments to an agent-shell buffer (or the kill ring when there is none).
;; The agent edits the files and writes its replies to a JSON file under
;; `agent-review-reply-directory'; once that file appears, the replies are
;; added to the comments and the next round starts.
;;
;; Two kinds of review:
;; - files: the given files with line numbers.  `d' toggles a diff against
;;   what the reviewer saw in the previous round.
;; - git: with no files, the working tree against the merge base with the
;;   default branch, plus untracked files.
;;
;; A session is keyed by its files (or by repository and branch), kept in
;; `agent-review-directory', and survives an Emacs restart.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'filenotify)
(require 'project)
(require 'subr-x)
(require 'text-property-search)

(defgroup agent-review nil
  "Review coding agent output in Emacs buffers."
  :group 'tools)

(defcustom agent-review-directory (locate-user-emacs-file "agent-review/")
  "Directory holding one subdirectory per review session."
  :type 'directory)

(defcustom agent-review-author
  (if (string-empty-p user-full-name) user-login-name user-full-name)
  "Author name put on the reviewer's comments and replies."
  :type 'string)

(defcustom agent-review-fontify-max-size 300000
  "Files larger than this many characters are shown without fontification."
  :type 'integer)

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

(defcustom agent-review-reply-directory "z-ai/"
  "Directory, relative to the review root, where the agent writes its replies.
Keep it out of version control; the files are deleted once read."
  :type 'string)

(cl-defstruct (agent-review-session
               (:constructor agent-review--make-session)
               (:copier nil))
  id mode root files (round 1) (state "reviewing") comments
  snapshot prev-snapshot prompt approved buffer)

(defvar agent-review--sessions (make-hash-table :test #'equal)
  "Live sessions by id.")

(defvar agent-review--reply-watches (make-hash-table :test #'equal)
  "Watches for the agent's reply file, as (DESCRIPTOR . TIMER) by session id.")

(defvar-local agent-review--session nil
  "Session shown in the current review buffer.")

(defvar-local agent-review--round-diff nil
  "Non-nil when a files review shows changes since the previous round.")

(defvar-local agent-review--watches nil
  "File notification descriptors of the current review buffer.")

(defvar-local agent-review--refresh-timer nil
  "Pending debounced refresh of the current review buffer.")

(defvar agent-review--fontify-cache nil
  "Fontified lines by absolute file name, bound while rendering.")

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

(defun agent-review--session-id (mode root files branch)
  "Return the session id for MODE over ROOT, FILES and git BRANCH."
  (substring
   (secure-hash 'sha1
                (string-join
                 (cons (symbol-name mode)
                       (if (eq mode 'git)
                           (list root (or branch ""))
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

(defun agent-review--session-file (id)
  "Return the JSON file of session ID."
  (expand-file-name (concat id "/review.json") agent-review-directory))

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
   :author (plist-get c :author) :round (plist-get c :round)
   :resolved (and (plist-get c :resolved) t)
   :drifted (and (plist-get c :drifted) t)
   :replies (and (plist-get c :replies)
                 (vconcat (mapcar (lambda (r)
                                    (list :author (plist-get r :author)
                                          :body (plist-get r :body)))
                                  (plist-get c :replies))))))

(defun agent-review--save (s)
  "Write session S to disk."
  (let ((file (agent-review--session-file (agent-review-session-id s))))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file file
        (json-insert
         (agent-review--obj
          :id (agent-review-session-id s)
          :mode (symbol-name (agent-review-session-mode s))
          :root (agent-review-session-root s)
          :files (vconcat (agent-review-session-files s))
          :round (agent-review-session-round s)
          :state (agent-review-session-state s)
          :comments (vconcat (mapcar #'agent-review--comment-json
                                     (agent-review-session-comments s)))
          :snapshot (agent-review--snapshot-json (agent-review-session-snapshot s))
          :prev_snapshot (agent-review--snapshot-json
                          (agent-review-session-prev-snapshot s))
          :prompt (agent-review-session-prompt s)
          :approved (and (agent-review-session-approved s) t)))))))

(defun agent-review--load (id)
  "Return session ID read from disk, or nil when it was never saved."
  (let ((file (agent-review--session-file id)))
    (when (file-readable-p file)
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
         :round (plist-get d :round)
         :state (plist-get d :state)
         :comments (plist-get d :comments)
         :snapshot (agent-review--snapshot-read (plist-get d :snapshot))
         :prev-snapshot (agent-review--snapshot-read (plist-get d :prev_snapshot))
         :prompt (plist-get d :prompt)
         :approved (plist-get d :approved))))))

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
    (dolist (dir (directory-files agent-review-directory nil "\\`[0-9a-f]+\\'"))
      (ignore-errors (agent-review--get dir))))
  (hash-table-values agent-review--sessions))

;;;; Files and git

(defun agent-review--read-file (file)
  "Return the contents of FILE, or nil when it cannot be read."
  (when (file-readable-p file)
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

(defun agent-review--git-base (root)
  "Return the commit the git review of ROOT diffs against."
  (let ((default
         (or (agent-review--git-try root "symbolic-ref" "--quiet" "--short"
                                    "refs/remotes/origin/HEAD")
             (cl-find-if (lambda (b)
                           (agent-review--git-try root "rev-parse" "--verify" "--quiet" b))
                         '("main" "master")))))
    (or (and default (agent-review--git-try root "merge-base" "HEAD" default))
        "HEAD")))

(defun agent-review--git-entries (root)
  "Return the entries of the git review of ROOT."
  (let ((diff (agent-review--git root "-c" "core.quotePath=false" "diff"
                                 "--no-color" "--no-ext-diff"
                                 "--src-prefix=a/" "--dst-prefix=b/"
                                 (agent-review--git-base root)))
        (untracked (split-string
                    (agent-review--git root "ls-files" "--others"
                                       "--exclude-standard" "-z")
                    "\0" t)))
    (append
     (agent-review--parse-diff diff)
     (mapcan (lambda (f)
               (when-let* ((content (agent-review--read-file (expand-file-name f root))))
                 (cons (list :kind 'file :file f :note "untracked")
                       (cl-loop for text in (agent-review--lines content)
                                for n from 1
                                collect (list :kind 'add :file f :new n :text text)))))
             untracked))))

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

(defun agent-review--fontified-line (s file n)
  "Return line N of FILE of session S fontified, or nil when unavailable."
  (let* ((abs (agent-review--abs s file))
         (lines (if (assoc abs agent-review--fontify-cache)
                    (cdr (assoc abs agent-review--fontify-cache))
                  (let ((v (when-let* ((content (agent-review--read-file abs)))
                             (agent-review--fontify abs content))))
                    (push (cons abs v) agent-review--fontify-cache)
                    v))))
    (and lines n (<= 1 n (length lines)) (aref lines (1- n)))))

(defun agent-review--file-entries (s abs)
  "Return the entries for the files review of ABS in session S."
  (let* ((name (agent-review--display-name s abs))
         (content (agent-review--read-file abs))
         (prev (cdr (assoc name (agent-review-session-prev-snapshot s)))))
    (cond
     ((null content)
      (list (list :kind 'file :file name :note "missing")))
     ((and agent-review--round-diff prev (not (equal prev content)))
      (cons (list :kind 'file :file name
                  :note (format "changes since round %d"
                                (1- (agent-review-session-round s))))
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
      (agent-review--git-entries (agent-review-session-root s))
    (mapcan (lambda (abs) (agent-review--file-entries s abs))
            (agent-review-session-files s))))

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

(defun agent-review--insert-line (s e diff-p)
  "Insert line entry E of session S; DIFF-P selects the diff gutter."
  (let* ((kind (plist-get e :kind))
         (file (plist-get e :file))
         (side (if (eq kind 'del) "old" "new"))
         (line (plist-get e (if (eq kind 'del) :old :new)))
         (sign (pcase kind ('add "+") ('del "-") (_ " ")))
         (gutter (if diff-p
                     (format "%5s %5s %s│ "
                             (or (plist-get e :old) "") (or (plist-get e :new) "") sign)
                   (format "%5d │ " line)))
         (body (concat (or (and (not (eq kind 'del))
                                (agent-review--fontified-line s file line))
                           (plist-get e :text))
                       "\n"))
         (body (pcase kind
                 ('add (agent-review--add-face body 'agent-review-added))
                 ('del (agent-review--add-face body 'agent-review-removed))
                 (_ body))))
    (insert (propertize (concat (agent-review--face gutter 'agent-review-line-number) body)
                        'agent-review-line
                        (list :file file :side side :line line :text (plist-get e :text))))))

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
         (comments (agent-review-session-comments s))
         (index (make-hash-table :test #'equal))
         (placed (make-hash-table :test #'equal))
         (diff-p (or (eq (agent-review-session-mode s) 'git) agent-review--round-diff))
         (agent-review--fontify-cache nil))
    (dolist (c (reverse comments))
      (when (plist-get c :file)
        (push c (gethash (agent-review--loc-key (plist-get c :file) (plist-get c :side)
                                                (plist-get c :end_line))
                         index))))
    (erase-buffer)
    (insert (agent-review--face
             (format "%s review of %s  (session %s)\n"
                     (if (eq (agent-review-session-mode s) 'git) "Git" "File")
                     (abbreviate-file-name (agent-review-session-root s))
                     (agent-review-session-id s))
             'bold))
    (let ((general (cl-remove-if (lambda (c) (plist-get c :file)) comments)))
      (when general
        (insert "\n" (agent-review--face "General comments\n" 'agent-review-file-header))
        (dolist (c general) (insert (agent-review--comment-block c)))))
    (dolist (e (agent-review--entries s))
      (pcase (plist-get e :kind)
        ('file
         (insert "\n" (propertize
                       (agent-review--face
                        (concat (plist-get e :file)
                                (if (plist-get e :note) (format "  (%s)" (plist-get e :note)) "")
                                "\n")
                        'agent-review-file-header)
                       'agent-review-file (plist-get e :file))))
        ('hunk
         (insert (agent-review--face (concat (plist-get e :text) "\n") 'agent-review-hunk)))
        (kind
         (agent-review--insert-line s e diff-p)
         (dolist (c (gethash (agent-review--loc-key
                              (plist-get e :file) (if (eq kind 'del) "old" "new")
                              (plist-get e (if (eq kind 'del) :old :new)))
                             index))
           (unless (gethash (plist-get c :id) placed)
             (puthash (plist-get c :id) t placed)
             (insert (agent-review--comment-block c)))))))
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
    (format " Round %d · %s · %d open │ c comment  C general  r reply  R resolve  e edit  D delete%s  N next round  C-c C-c finish"
            (agent-review-session-round s)
            (cond
             ((equal (agent-review-session-state s) "reviewing") "reviewing")
             ((agent-review-session-approved s) "approved")
             (t "waiting for the agent's replies"))
            (length (agent-review--unresolved (agent-review-session-comments s)))
            (if (eq (agent-review-session-mode s) 'files) "  d round diff" ""))))

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
                                  (agent-review--render)))))))))

(defun agent-review--watch ()
  "Redraw the current files review whenever one of its files changes."
  (let* ((s agent-review--session)
         (buf (current-buffer))
         (files (agent-review-session-files s))
         (truenames (mapcar #'file-truename files)))
    (when (eq (agent-review-session-mode s) 'files)
      ;; Agents often replace a file by renaming over it, which ends a
      ;; watch on the file itself, so watch the directories instead.
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
  "Stop the file watches and pending refresh of the current buffer."
  (mapc #'file-notify-rm-watch agent-review--watches)
  (setq agent-review--watches nil)
  (when (timerp agent-review--refresh-timer)
    (cancel-timer agent-review--refresh-timer)))

(defun agent-review--ensure-buffer (s)
  "Return the review buffer of session S, creating it when needed."
  (let ((buf (agent-review-session-buffer s)))
    (unless (buffer-live-p buf)
      (setq buf (generate-new-buffer
                 (format "*agent-review: %s*"
                         (if (eq (agent-review-session-mode s) 'git)
                             (concat "git "
                                     (file-name-nondirectory
                                      (directory-file-name (agent-review-session-root s))))
                           (mapconcat #'file-name-nondirectory
                                      (agent-review-session-files s) " ")))))
      (with-current-buffer buf
        (agent-review-mode)
        (setq agent-review--session s)
        (agent-review--watch)
        (agent-review--render))
      (setf (agent-review-session-buffer s) buf))
    buf))

(defun agent-review--changed (s)
  "Save session S and redraw its buffer."
  (agent-review--save s)
  (let ((buf (agent-review-session-buffer s)))
    (when (buffer-live-p buf)
      (with-current-buffer buf (agent-review--render)))))

(defun agent-review--display (s)
  "Show the review buffer of session S."
  (pop-to-buffer (agent-review--ensure-buffer s)))

(defun agent-review--open (mode root files)
  "Return the session for MODE over ROOT and FILES, creating it when new."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (branch (and (eq mode 'git) (agent-review--git-try root "rev-parse" "--abbrev-ref" "HEAD")))
         (id (agent-review--session-id mode root files branch)))
    (or (gethash id agent-review--sessions)
        (when-let* ((loaded (agent-review--load id)))
          (agent-review--register loaded))
        (let ((new (agent-review--make-session :id id :mode mode :root root :files files)))
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
  (agent-review--changed s)
  (message "agent-review: round %d is ready" (agent-review-session-round s)))

;;;; Agent replies

(defun agent-review--reply-file (s)
  "Return the file the agent writes its replies to session S in."
  (expand-file-name (format "agent-review-%s.json" (agent-review-session-id s))
                    (expand-file-name agent-review-reply-directory
                                      (agent-review-session-root s))))

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
                                    :author agent-review-author
                                    :round (agent-review-session-round s))
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

(defun agent-review-toggle-round-diff ()
  "Toggle showing what changed in the files since the previous round."
  (interactive)
  (let ((s (agent-review--current)))
    (unless (eq (agent-review-session-mode s) 'files)
      (user-error "A git review already shows a diff"))
    (unless (agent-review-session-prev-snapshot s)
      (user-error "There is no previous round"))
    (setq agent-review--round-diff (not agent-review--round-diff))
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
      (user-error "Round %d is already finished" (agent-review-session-round s)))
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
      (message "agent-review: round %d %s; %s"
               (agent-review-session-round s)
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
      (user-error "Round %d is still being reviewed" (agent-review-session-round s)))
    (agent-review--read-replies s)
    (agent-review--start-round s)))

(defun agent-review--describe (s)
  "Return a one-line description of session S."
  (format "%s  %s  round %d  %s  (%d open)"
          (agent-review-session-id s)
          (if (eq (agent-review-session-mode s) 'git)
              (concat "git " (abbreviate-file-name (agent-review-session-root s)))
            (mapconcat (lambda (f) (agent-review--display-name s f))
                       (agent-review-session-files s) ", "))
          (agent-review-session-round s)
          (agent-review-session-state s)
          (length (agent-review--unresolved (agent-review-session-comments s)))))

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
  "d" #'agent-review-toggle-round-diff
  "N" #'agent-review-new-round
  "RET" #'agent-review-visit
  "C-c C-c" #'agent-review-finish)

(define-derived-mode agent-review-mode special-mode "Agent-Review"
  "Major mode for reviewing agent output.

\\{agent-review-mode-map}"
  (setq-local header-line-format '(:eval (agent-review--header-line)))
  (setq-local revert-buffer-function (lambda (&rest _) (agent-review--render)))
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
