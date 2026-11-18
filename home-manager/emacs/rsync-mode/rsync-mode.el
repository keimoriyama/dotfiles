;;; rsync-mode.el --- Synchronize projects with rsync  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1") (spinner "1.7.1"))
;; Keywords: comm

;;; Commentary:

;; Project-oriented rsync commands and a save hook.  Each process owns its
;; sentinel closure, so completing a sync never selects a deleted source buffer.

;;; Code:

(require 'cl-lib)
(require 'seq)
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
  (mapcar (lambda (pattern)
            (format "--exclude=%s" (shell-quote-argument pattern)))
          (delete-dups
           (append rsync-excluded-dirs rsync-default-excluded-dirs))))

(defun rsync--build-args (remote-path excludes local-path &optional dry-run file)
  "Build rsync arguments for REMOTE-PATH and LOCAL-PATH.
EXCLUDES is a list of command arguments.  When DRY-RUN is non-nil, do not
modify the destination.  When FILE is non-nil, synchronize only that path."
  (let ((remote (string-join
                 (mapcar #'shell-quote-argument
                         (split-string remote-path ":"))
                 ":")))
    (seq-filter
     #'identity
     (append
      (list (if file "-avR" "-av")
            (and dry-run "--dry-run"))
      excludes
      (list (shell-quote-argument
             (if file
                 (concat (file-name-as-directory local-path) "./" file)
               local-path))
            remote)))))

(defun rsync--process-for (remote-path)
  "Return a live process for REMOTE-PATH in the current buffer."
  (cl-find-if
   (lambda (process)
     (and (process-live-p process)
          (equal (process-get process 'rsync-remote-path) remote-path)))
   rsync--processes))

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

(defun rsync--make-process-sentinel (source)
  "Return a process sentinel that safely refers to SOURCE."
  (lambda (process event)
    (rsync--finish-process source process event)))

(defun rsync--prepare-output-buffer (remote-path)
  "Return a process output buffer initialized for REMOTE-PATH."
  (with-current-buffer (get-buffer-create
                        (rsync--get-rsync-buffer-name remote-path))
    (goto-char (point-max))
    (skip-chars-backward "\n[:space:]")
    (insert (concat "\n\n" (time-stamp-string) "\n"))
    (current-buffer)))

(defun rsync--run (remote-path excludes local-path &optional dry-run file)
  "Synchronize LOCAL-PATH to REMOTE-PATH with EXCLUDES.
DRY-RUN and FILE are passed to `rsync--build-args'."
  (if (rsync--process-for remote-path)
      (message "An rsync process for %s is already running" remote-path)
    (let* ((source (current-buffer))
           (output (rsync--prepare-output-buffer remote-path))
           (process
            (apply #'start-process
                   "rsync" output "rsync"
                   (rsync--build-args remote-path excludes local-path
                                      dry-run file))))
      (process-put process 'rsync-remote-path remote-path)
      (set-process-query-on-exit-flag process nil)
      (set-process-sentinel process (rsync--make-process-sentinel source))
      (push process rsync--processes)
      (setq rsync--process process)
      (rsync--start-spinner)
      process)))

;;;###autoload
(defun rsync-all (&optional dry-run file)
  "Synchronize the current project to every configured destination.
When DRY-RUN is non-nil, do not modify destinations.  FILE limits the sync to
one project-relative path."
  (interactive)
  (unless rsync-local-path
    (user-error "No local path configured for rsync-mode"))
  (dolist (remote-path rsync-remote-paths)
    (rsync--run remote-path (rsync--get-excludes) rsync-local-path
                dry-run file)))

(defun rsync--select-remote ()
  "Prompt for one configured remote destination."
  (completing-read "Rsync project to: " rsync-remote-paths nil t))

;;;###autoload
(defun rsync (&optional dry-run file)
  "Synchronize the current project to one selected destination.
When DRY-RUN is non-nil, do not modify the destination.  FILE limits the sync
to one project-relative path."
  (interactive)
  (unless rsync-local-path
    (user-error "No local path configured for rsync-mode"))
  (rsync--run (rsync--select-remote) (rsync--get-excludes)
              rsync-local-path dry-run file))

(provide 'rsync-mode)
;;; rsync-mode.el ends here
