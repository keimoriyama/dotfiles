;;; rsync-mode-tests.el --- Tests for local rsync-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "rsync-mode/rsync-mode.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun rsync-mode-tests--wait-until (predicate)
  "Process output until PREDICATE returns non-nil, failing after 10 seconds."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (should (funcall predicate))))

(defun rsync-mode-tests--read-file (file)
  "Return the contents of FILE, or nil when it does not exist."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defmacro rsync-mode-tests--with-fake-rsync (script &rest body)
  "Run BODY with an `rsync' executable on `exec-path' that runs SCRIPT.
SCRIPT is a sh fragment; the variable LOG names a file it may append to.
BODY sees the bound variables `root' and `log'."
  (declare (indent 1))
  `(let* ((root (file-name-as-directory (make-temp-file "rsync-mode-" t)))
          (log (expand-file-name "log" root))
          (bin (expand-file-name "bin" root))
          (exec-path (cons bin exec-path)))
     (unwind-protect
         (progn
           (make-directory bin)
           (with-temp-file (expand-file-name "rsync" bin)
             (insert "#!/bin/sh\nLOG=" (shell-quote-argument log) "\n"
                     ,script "\n"))
           (set-file-modes (expand-file-name "rsync" bin) #o755)
           (with-temp-buffer
             ,@body))
       (dolist (process (process-list))
         (when (equal (process-get process 'rsync-local-path) root)
           (delete-process process)))
       (dolist (buffer (buffer-list))
         (when (string-prefix-p (format "*rsync to %s" root)
                                (buffer-name buffer))
           (kill-buffer buffer)))
       (delete-directory root t))))

(defun rsync-mode-tests--run-count (log)
  "Return how many fake rsync runs have been recorded in LOG."
  (let ((contents (rsync-mode-tests--read-file log)))
    (if contents (cl-count ?\n contents) 0)))

(ert-deftest rsync-mode-sentinel-ignores-a-deleted-source-buffer ()
  "A process may finish after its Magit or file buffer has been killed."
  (let* ((source (generate-new-buffer " *rsync-deleted-source*"))
         (sentinel (rsync--make-process-sentinel source))
         (process (make-process :name "rsync-mode-test" :command '("true")
                                :noquery t :sentinel #'ignore)))
    (kill-buffer source)
    (rsync-mode-tests--wait-until (lambda () (not (process-live-p process))))
    (funcall sentinel process "finished\n")
    (should-not (buffer-live-p source))))

(ert-deftest rsync-mode-sentinels-clean-up-only-their-source-buffer ()
  "Concurrent processes keep independent source-buffer cleanup state."
  (let ((first (generate-new-buffer " *rsync-first-source*"))
        (second (generate-new-buffer " *rsync-second-source*"))
        (process-a 'process-a)
        (process-b 'process-b))
    (unwind-protect
        (progn
          (with-current-buffer first
            (setq rsync--processes (list process-a)))
          (with-current-buffer second
            (setq rsync--processes (list process-b)))
          (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
                    ((symbol-function 'rsync--stop-spinner) #'ignore))
            (rsync--finish-process first process-a "finished\n"))
          (with-current-buffer first
            (should-not rsync--processes))
          (with-current-buffer second
            (should (equal rsync--processes (list process-b)))))
      (kill-buffer first)
      (kill-buffer second))))

(ert-deftest rsync-mode-build-args-passes-paths-unquoted ()
  "Paths with spaces and user@host prefixes reach rsync verbatim.
rsync is started without a shell, so shell quoting would become part of
the file name."
  (let ((args (rsync--build-args "me@host:dir with space" nil "/src dir/")))
    (should (member "/src dir/" args))
    (should (equal (car (last args)) "me@host:dir with space"))
    (should-not (cl-some (lambda (arg) (string-search "\\" arg)) args))))

(ert-deftest rsync-mode-build-args-limits-a-file-sync-to-a-relative-path ()
  "A single-file sync keeps the project-relative path for --relative."
  (let ((args (rsync--build-args "host:dst" nil "/src dir" t "sub/a b.txt")))
    (should (equal (car args) "-avR"))
    (should (member "--dry-run" args))
    (should (member "/src dir/./sub/a b.txt" args))))

(ert-deftest rsync-mode-excludes-pass-patterns-unquoted ()
  "Exclusion patterns with spaces are passed literally and deduplicated."
  (with-temp-buffer
    (let ((rsync-default-excluded-dirs '("build dir/" ".git/")))
      (setq rsync-excluded-dirs '(".git/"))
      (should (equal (rsync--get-excludes)
                     '("--exclude=.git/" "--exclude=build dir/"))))))

(ert-deftest rsync-mode-syncs-a-local-directory-containing-spaces ()
  "A real rsync copies between directories whose names contain spaces."
  (skip-unless (executable-find "rsync"))
  (let ((root (file-name-as-directory (make-temp-file "rsync-mode-real-" t))))
    (unwind-protect
        (let ((src (expand-file-name "my src/" root))
              (dst (expand-file-name "my dst" root)))
          (make-directory src)
          (write-region "x" nil (expand-file-name "a b.txt" src) nil 'silent)
          (with-temp-buffer
            (setq rsync-local-path src
                  rsync-remote-paths (list dst))
            (rsync-all)
            (let ((process rsync--process))
              (rsync-mode-tests--wait-until
               (lambda () (not (process-live-p process))))
              (should (zerop (process-exit-status process)))))
          (should (file-exists-p (expand-file-name "a b.txt" dst))))
      (dolist (buffer (buffer-list))
        (when (string-prefix-p (format "*rsync to %s" root) (buffer-name buffer))
          (kill-buffer buffer)))
      (delete-directory root t))))

(ert-deftest rsync-mode-runs-rsync-over-a-pipe ()
  "rsync gets no pty, so an ssh password prompt fails instead of hanging."
  (rsync-mode-tests--with-fake-rsync "exit 0"
    (let (connection-type)
      (cl-letf* ((make-process-orig (symbol-function 'make-process))
                 ((symbol-function 'make-process)
                  (lambda (&rest plist)
                    (setq connection-type (plist-get plist :connection-type))
                    (apply make-process-orig plist))))
        (setq rsync-local-path root
              rsync-remote-paths (list (concat root "dst")))
        (rsync-all))
      (should (eq connection-type 'pipe)))))

(ert-deftest rsync-mode-expands-a-tilde-in-the-local-path ()
  "rsync does not expand ~, so the local path is expanded beforehand."
  (rsync-mode-tests--with-fake-rsync "printf '%s\\n' \"$@\" > \"$LOG\""
    (let ((process-environment (cons (concat "HOME=" root) process-environment))
          (default-directory root))
      (setq rsync-local-path "~/src/"
            rsync-remote-paths (list (concat root "dst")))
      (rsync-all)
      (rsync-mode-tests--wait-until (lambda () (file-exists-p log)))
      (should (string-search (concat "\n" root "src/\n")
                             (concat "\n" (rsync-mode-tests--read-file log))))
      (should-not (string-search "~" (rsync-mode-tests--read-file log))))))

(ert-deftest rsync-mode-save-during-a-sync-runs-once-more-afterwards ()
  "Requests made while a sync runs are coalesced into one follow-up sync."
  (rsync-mode-tests--with-fake-rsync "echo run >> \"$LOG\"; sleep 0.5"
    (setq rsync-local-path root
          rsync-remote-paths (list (concat root "dst")))
    (rsync-all)
    (rsync-all)
    (rsync-all)
    (rsync-mode-tests--wait-until
     (lambda () (= (rsync-mode-tests--run-count log) 2)))
    (rsync-mode-tests--wait-until
     (lambda () (not (rsync--process-for (concat root "dst") root))))
    (should (= (rsync-mode-tests--run-count log) 2))))

(ert-deftest rsync-mode-queued-sync-runs-once-after-a-failure ()
  "A failed sync still starts the queued one, and only once."
  (rsync-mode-tests--with-fake-rsync "echo run >> \"$LOG\"; sleep 0.3; exit 12"
    (setq rsync-local-path root
          rsync-remote-paths (list (concat root "dst")))
    (rsync-all)
    (rsync-all)
    (rsync-mode-tests--wait-until
     (lambda () (= (rsync-mode-tests--run-count log) 2)))
    (rsync-mode-tests--wait-until
     (lambda () (not (rsync--process-for (concat root "dst") root))))
    (should (= (rsync-mode-tests--run-count log) 2))))

(ert-deftest rsync-mode-queued-sync-runs-after-its-buffer-is-killed ()
  "Killing the buffer that queued a sync does not cancel the sync."
  (rsync-mode-tests--with-fake-rsync "echo run >> \"$LOG\"; sleep 0.3"
    (let ((source (generate-new-buffer " *rsync-queued-source*"))
          buffers-before)
      (with-current-buffer source
        (setq rsync-local-path root
              rsync-remote-paths (list (concat root "dst")))
        (rsync-all)
        (rsync-all))
      (kill-buffer source)
      (setq buffers-before (buffer-list))
      (rsync-mode-tests--wait-until
       (lambda () (= (rsync-mode-tests--run-count log) 2)))
      (should (cl-subsetp (buffer-list) buffers-before)))))

(ert-deftest rsync-mode-dry-run-during-a-sync-is-not-queued ()
  "A dry run changes nothing, so it is skipped rather than queued."
  (rsync-mode-tests--with-fake-rsync "echo run >> \"$LOG\"; sleep 0.3"
    (setq rsync-local-path root
          rsync-remote-paths (list (concat root "dst")))
    (rsync-all)
    (let ((process rsync--process))
      (rsync-all t)
      (should-not (process-get process 'rsync-queued)))))

(ert-deftest rsync-mode-merging-requests-keeps-different-files-apart ()
  "A newer request for a queued file replaces it; other files queue after it."
  (let ((a '(buf "host:dst" ("--exclude=x") "/src/" "a.txt"))
        (a2 '(buf "host:dst" ("--exclude=z") "/src/" "a.txt"))
        (b '(buf "host:dst" ("--exclude=y") "/src/" "b.txt")))
    (should (equal (rsync--merge-requests nil a) (list a)))
    (should (equal (rsync--merge-requests (list a) a2) (list a2)))
    (should (equal (rsync--merge-requests (list a b) a2) (list b a2)))))

(ert-deftest rsync-mode-queued-file-syncs-never-widen-to-a-project-sync ()
  "Saving two files during a sync sends each file alone, never the whole project."
  (rsync-mode-tests--with-fake-rsync "echo \"$1\" >> \"$LOG\"; sleep 0.3"
    (setq rsync-local-path root
          rsync-remote-paths (list (concat root "dst")))
    (rsync-all)
    (rsync-all nil "a.txt")
    (rsync-all nil "b.txt")
    (rsync-mode-tests--wait-until
     (lambda () (= (rsync-mode-tests--run-count log) 3)))
    (rsync-mode-tests--wait-until
     (lambda () (not (rsync--process-for (concat root "dst") root))))
    (should (equal (rsync-mode-tests--read-file log) "-av\n-avR\n-avR\n"))))

(ert-deftest rsync-mode-rsync-all-requires-a-remote ()
  "Without destinations, rsync-all reports an error instead of doing nothing."
  (with-temp-buffer
    (setq rsync-local-path "/src/"
          rsync-remote-paths nil)
    (should-error (rsync-all) :type 'user-error)))

(ert-deftest rsync-mode-rsync-rejects-an-empty-destination ()
  "An empty answer to the destination prompt is not passed to rsync."
  (with-temp-buffer
    (setq rsync-local-path "/src/"
          rsync-remote-paths '("host:dst"))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
      (should-error (rsync) :type 'user-error))))

(provide 'rsync-mode-tests)
;;; rsync-mode-tests.el ends here
