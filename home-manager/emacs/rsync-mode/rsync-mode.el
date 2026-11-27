;;; rsync-mode.el --- Synchronize projects with rsync  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1") (spinner "1.7.1"))
;; Keywords: comm

;;; Commentary:

;; Project-oriented rsync commands and a save hook.  Each process owns its
;; sentinel closure, so completing a sync never selects a deleted source buffer.

;;; Code:

(require 'cl-lib)
(require 'spinner)
(require 'subr-x)
(require 'time-stamp)

(defgroup rsync nil
  "Synchronize projects with rsync."
  :group 'convenience
  :prefix "rsync-")

(defcustom rsync-default-excluded-dirs nil
  "Rsync patterns excluded from every project."
  :group 'rsync
  :type '(repeat string))

(defcustom rsync-sync-on-save nil
  "When non-nil, synchronize the current project after saving."
  :group 'rsync
  :type 'boolean)

(defvar-local rsync-local-path nil
  "Local project root passed to rsync.")

(defvar-local rsync-remote-paths nil
  "Remote destinations in host:path form.")

(defvar-local rsync-excluded-dirs nil
  "Project-specific rsync exclusion patterns.")

(defvar-local rsync--process nil
  "Most recently started live rsync process, for compatibility.")

(defvar-local rsync--processes nil
  "Rsync processes started from the current buffer.")

(defvar-local rsync--spinner nil
  "Spinner displayed while this buffer has active rsync processes.")

(defconst rsync--lighter
  '(" rsync" (:eval (and rsync--spinner (spinner-print rsync--spinner))))
  "Mode-line indicator for `rsync-mode'.")

(defvar rsync-mode)

(defun rsync--start-spinner ()
  "Start the current buffer's rsync spinner."
  (when rsync-mode
    (unless rsync--spinner
      (setq rsync--spinner (spinner-create 'progress-bar-filled t)))
    (spinner-start rsync--spinner)))

(defun rsync--stop-spinner ()
  "Stop the current buffer's rsync spinner."
  (when rsync--spinner
    (spinner-stop rsync--spinner)))

;;;###autoload
(define-minor-mode rsync-mode
  "Synchronize the configured project with rsync."
  :init-value nil
  :lighter rsync--lighter
  :group 'rsync
  (cond
   ((and rsync-mode (null rsync-remote-paths))
    (setq rsync-mode nil)
    (user-error "No remote configuration for rsync-mode"))
   (rsync-mode
    (when rsync-sync-on-save
      (add-hook 'after-save-hook #'rsync-all 0 t)))
   (t
    (remove-hook 'after-save-hook #'rsync-all t))))

(defun rsync--get-hostname (path)
  "Return the hostname portion of remote PATH."
  (let* ((user-host (car (split-string path ":")))
         (at (string-match "@" user-host)))
    (if at (substring user-host (1+ at)) user-host)))

(defun rsync--get-rsync-buffer-name (remote-path)
  "Return the process buffer name for REMOTE-PATH."
  (format "*rsync to %s*" (rsync--get-hostname remote-path)))

(defun rsync--get-excludes ()
  "Return command arguments for global and project exclusions."
  (mapcar (lambda (pattern) (concat "--exclude=" pattern))
          (delete-dups
           (append rsync-excluded-dirs rsync-default-excluded-dirs))))

(defun rsync--build-args (remote-path excludes local-path &optional dry-run file)
  "Build rsync arguments for REMOTE-PATH and LOCAL-PATH.
EXCLUDES is a list of command arguments.  When DRY-RUN is non-nil, do not
modify the destination.  When FILE is non-nil, synchronize only that path.
The arguments go to rsync directly, not through a shell, so they are not
quoted."
  (append
   (list (if file "-avR" "-av"))
   (and dry-run (list "--dry-run"))
   excludes
   (list (if file
             (concat (file-name-as-directory local-path) "./" file)
           local-path)
         remote-path)))

(defun rsync--process-for (remote-path local-path)
  "Return the live rsync process syncing LOCAL-PATH to REMOTE-PATH.
Every buffer of a project shares one process per destination, so the
search covers all processes rather than those of the current buffer."
  (cl-find-if
   (lambda (process)
     (and (process-live-p process)
          (equal (process-get process 'rsync-remote-path) remote-path)
          (equal (process-get process 'rsync-local-path) local-path)))
   (process-list)))

(defun rsync--finish-process (source process event)
  "Clean up PROCESS from SOURCE after receiving EVENT."
  (when (buffer-live-p source)
    (with-current-buffer source
      (setq rsync--processes (delq process rsync--processes)
            rsync--process (cl-find-if #'process-live-p rsync--processes))
      (unless rsync--process
        (rsync--stop-spinner))))
  (if (string-equal event "finished\n")
      (message "Rsync complete.")
    (message "Rsync process received abnormal event %s" event)))

(defun rsync--merge-requests (queued request)
  "Return the request list QUEUED with REQUEST added at its end.
Each request is (SOURCE REMOTE-PATH EXCLUDES LOCAL-PATH FILE).  A queued
request for the same FILE is replaced, since one run sends its latest state."
  ;; Different files stay separate requests: widening them to a project
  ;; sync would run the user's mirroring arguments, such as --delete, on a
  ;; save that only asked for one file.
  (append (cl-remove (nth 4 request) queued
                     :key (lambda (r) (nth 4 r)) :test #'equal)
          (list request)))

(defun rsync--run-queued (process)
  "Start the syncs queued on PROCESS while it was running, one at a time.
The queued syncs start even when PROCESS failed: the failure may have been
transient, and the newer changes still need sending.  Each request runs
once, so a failing destination cannot loop."
  (when-let* ((queue (process-get process 'rsync-queued)))
    (process-put process 'rsync-queued nil)
    (pcase-let ((`(,source ,remote-path ,excludes ,local-path ,file) (car queue)))
      (if (buffer-live-p source)
          (with-current-buffer source
            (rsync--run remote-path excludes local-path nil file))
        (with-temp-buffer
          (rsync--run remote-path excludes local-path nil file)))
      (when-let* ((next (rsync--process-for remote-path
                                            (expand-file-name local-path))))
        (dolist (request (cdr queue))
          (rsync--queue next request nil))))))

(defun rsync--make-process-sentinel (source)
  "Return a process sentinel that safely refers to SOURCE."
  (lambda (process event)
    (rsync--finish-process source process event)
    (unless (process-live-p process)
      (rsync--run-queued process))))

(defun rsync--prepare-output-buffer (remote-path)
  "Return a process output buffer initialized for REMOTE-PATH."
  (with-current-buffer (get-buffer-create
                        (rsync--get-rsync-buffer-name remote-path))
    (goto-char (point-max))
    (skip-chars-backward "\n[:space:]")
    (insert (concat "\n\n" (time-stamp-string) "\n"))
    (current-buffer)))

(defun rsync--queue (process request dry-run)
  "Queue REQUEST to run after PROCESS finishes, unless it is a DRY-RUN.
A dry run changes nothing, so skipping it loses no work."
  (if dry-run
      (message "An rsync process for %s is already running" (nth 1 request))
    (process-put process 'rsync-queued
                 (rsync--merge-requests (process-get process 'rsync-queued)
                                        request))
    (message "Rsync to %s is running; queued another sync" (nth 1 request))))

(defun rsync--start (remote-path excludes local-path dry-run file)
  "Start rsync from LOCAL-PATH to REMOTE-PATH and return the process.
See `rsync--build-args' for EXCLUDES, DRY-RUN and FILE."
  (let* ((source (current-buffer))
         ;; A pipe leaves ssh without a terminal, so a password or host-key
         ;; prompt fails immediately instead of waiting forever on a pty
         ;; nobody reads.
         (process
          (make-process
           :name "rsync"
           :buffer (rsync--prepare-output-buffer remote-path)
           :command (cons "rsync"
                          (rsync--build-args remote-path excludes local-path
                                             dry-run file))
           :connection-type 'pipe
           :noquery t
           :sentinel (rsync--make-process-sentinel source))))
    (process-put process 'rsync-remote-path remote-path)
    (process-put process 'rsync-local-path local-path)
    (push process rsync--processes)
    (setq rsync--process process)
    (rsync--start-spinner)
    process))

(defun rsync--run (remote-path excludes local-path &optional dry-run file)
  "Synchronize LOCAL-PATH to REMOTE-PATH with EXCLUDES.
DRY-RUN and FILE are passed to `rsync--build-args'.  While a sync to the
same destination is running, queue the request instead of dropping it."
  (let* ((local-path (expand-file-name local-path))
         (running (rsync--process-for remote-path local-path)))
    (if running
        (rsync--queue running
                      (list (current-buffer) remote-path excludes local-path
                            file)
                      dry-run)
      (rsync--start remote-path excludes local-path dry-run file))))

(defun rsync--check-local-path ()
  "Signal a `user-error' unless the current buffer has a local path."
  (unless rsync-local-path
    (user-error "No local path configured for rsync-mode")))

;;;###autoload
(defun rsync-all (&optional dry-run file)
  "Synchronize the current project to every configured destination.
When DRY-RUN is non-nil, do not modify destinations.  FILE limits the sync to
one project-relative path."
  (interactive)
  (rsync--check-local-path)
  (unless rsync-remote-paths
    (user-error "No remote configuration for rsync-mode"))
  (dolist (remote-path rsync-remote-paths)
    (rsync--run remote-path (rsync--get-excludes) rsync-local-path
                dry-run file)))

(defun rsync--select-remote ()
  "Prompt for one configured remote destination."
  (let ((remote (completing-read "Rsync project to: "
                                 rsync-remote-paths nil t)))
    (when (string-empty-p remote)
      (user-error "No rsync destination selected"))
    remote))

;;;###autoload
(defun rsync (&optional dry-run file)
  "Synchronize the current project to one selected destination.
When DRY-RUN is non-nil, do not modify the destination.  FILE limits the sync
to one project-relative path."
  (interactive)
  (rsync--check-local-path)
  (rsync--run (rsync--select-remote) (rsync--get-excludes)
              rsync-local-path dry-run file))

(provide 'rsync-mode)
;;; rsync-mode.el ends here
