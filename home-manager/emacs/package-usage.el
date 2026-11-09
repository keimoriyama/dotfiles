;;; package-usage.el --- Record how often installed packages are used -*- lexical-binding: t; -*-

;;; Commentary:

;; Count the commands run and the major modes entered, and note which
;; installed packages get loaded in each session.  The counts accumulate
;; in `package-usage-file' across sessions, and
;; `package-usage-write-report' turns them into a per-package JSON report
;; meant to be read by an AI agent when pruning unused packages.
;;
;; Commands are recorded by symbol and attributed to a package only when
;; the report is built, so the hot path stays a single hash table update.
;; Enable with (package-usage-mode 1).

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'package)
(require 'subr-x)

(defgroup package-usage nil
  "Record how often installed packages are used."
  :group 'package)

(defcustom package-usage-file
  (expand-file-name "package-usage.eld" user-emacs-directory)
  "File the accumulated counts are stored in."
  :type 'file)

(defcustom package-usage-report-file
  (expand-file-name "package-usage-report.json" user-emacs-directory)
  "File `package-usage-write-report' writes to."
  :type 'file)

(defcustom package-usage-save-interval
  300
  "Seconds between saves of the pending counts."
  :type 'integer)

(defcustom package-usage-top-commands
  5
  "Number of most used commands listed per package in the report."
  :type 'integer)

(defconst package-usage-report-notes
  ["command_count counts interactive commands defined by the package, however they were invoked (key, M-x, remap)."
   "mode_count counts how often a major mode defined by the package was turned on in a buffer."
   "loaded_sessions counts Emacs sessions in which any file of the package was loaded. Packages that work only through hooks, timers or as libraries (completion UIs, mode lines, themes) show up here but not in the counts."
   "A package whose required_by is non-empty is a dependency of another installed package and cannot be removed on its own."
   "Packages are installed through Nix; the explicitly requested ones are listed in home-manager/emacs/epkgs.nix and their configuration lives in home-manager/emacs/init.org."]
  "Guidance embedded at the top of the report.")

(defvar package-usage--pending-commands (make-hash-table :test #'eq)
  "Commands run since the last save, mapped to (COUNT . LAST-EPOCH).")

(defvar package-usage--pending-modes (make-hash-table :test #'eq)
  "Major modes entered since the last save, mapped to (COUNT . LAST-EPOCH).")

(defvar package-usage--pending-sessions 0
  "Sessions started since the last save.")

(defvar package-usage--session-counted nil
  "Non-nil once this Emacs process has been counted as a session.")

(defvar package-usage--counted-packages nil
  "Packages whose load has been counted for this session.")

(defvar package-usage--timer nil
  "Timer that periodically saves the pending counts.")

;;;; Recording

(defun package-usage--now ()
  "Return the current time in seconds since the epoch."
  (time-convert nil 'integer))

(defun package-usage--bump (table key)
  "Increment KEY in TABLE and stamp it with the current time."
  (let ((entry (gethash key table)))
    (puthash key (cons (1+ (or (car entry) 0)) (package-usage--now)) table)))

(defun package-usage--record-command ()
  "Count `this-command'."
  (when (and this-command (symbolp this-command))
    (package-usage--bump package-usage--pending-commands this-command)))

(defun package-usage--record-mode ()
  "Count `major-mode'."
  (package-usage--bump package-usage--pending-modes major-mode))

;;;; Mapping files to packages

(defun package-usage--package-dirs ()
  "Return an alist of (DIRECTORY . PACKAGE) for the installed packages."
  (cl-loop for (name desc) in package-alist
           for dir = (package-desc-dir desc)
           when (stringp dir)
           collect (cons (file-name-as-directory dir) name)))

(defun package-usage--file-package (file dirs)
  "Return the package in DIRS that FILE belongs to, or nil."
  ;; Paths are compared without `file-truename': Nix links each file of a
  ;; package directory to its own store path, so resolving links would
  ;; move FILE out of the directory it was loaded from.
  (when (stringp file)
    (cdr (cl-find-if (lambda (dir) (string-prefix-p (car dir) file)) dirs))))

(defun package-usage--symbol-package (symbol dirs)
  "Return the package in DIRS that defines the function SYMBOL, or nil."
  (package-usage--file-package (symbol-file symbol 'defun) dirs))

(defun package-usage--loaded-packages (dirs)
  "Return the packages in DIRS that have a file in `load-history'."
  (delete-dups
   (delq nil (mapcar (lambda (entry) (package-usage--file-package (car entry) dirs))
                     load-history))))

;;;; Persistence

(defun package-usage--merge-counts (alist table)
  "Return ALIST of (KEY COUNT . LAST) with the entries of TABLE added in.
TABLE maps KEY to (COUNT . LAST).  Counts are summed and the later LAST
is kept."
  (let ((merged (copy-tree alist)))
    (maphash
     (lambda (key value)
       (let ((cell (assq key merged)))
         (if cell
             (setcdr cell (cons (+ (cadr cell) (car value))
                                (max (cddr cell) (cdr value))))
           (push (cons key (cons (car value) (cdr value))) merged))))
     table)
    merged))

(defun package-usage--merge (data &rest pending)
  "Return DATA with the PENDING counts added in.
PENDING is a plist of :commands and :modes hash tables, :sessions, the
:newly-loaded packages to count a session for, the :loaded packages to
stamp, and :now."
  (let* ((now (plist-get pending :now))
         (loaded (copy-tree (plist-get data :loaded))))
    (dolist (pkg (plist-get pending :loaded))
      (let ((cell (assq pkg loaded))
            (new (if (memq pkg (plist-get pending :newly-loaded)) 1 0)))
        (if cell
            (setcdr cell (cons (+ (cadr cell) new) now))
          (push (cons pkg (cons new now)) loaded))))
    (list :version 1
          :since (or (plist-get data :since) now)
          :sessions (+ (or (plist-get data :sessions) 0)
                       (plist-get pending :sessions))
          :commands (package-usage--merge-counts (plist-get data :commands)
                                                 (plist-get pending :commands))
          :modes (package-usage--merge-counts (plist-get data :modes)
                                              (plist-get pending :modes))
          :loaded loaded)))

(defun package-usage--read (file)
  "Return the data stored in FILE, or nil when FILE is missing or empty."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (read (current-buffer))
        (end-of-file nil)))))

(defun package-usage--write (file data)
  "Write DATA to FILE."
  (let ((print-length nil)
        (print-level nil))
    (with-temp-file file
      (insert ";;; -*- lisp-data -*-\n")
      (prin1 data (current-buffer))
      (insert "\n"))))

(defun package-usage-save ()
  "Add the counts recorded since the last save to `package-usage-file'.
The file is re-read first so that several Emacs processes add up
instead of overwriting each other."
  (interactive)
  (let* ((loaded (package-usage--loaded-packages (package-usage--package-dirs)))
         (newly-loaded (cl-set-difference loaded package-usage--counted-packages)))
    (package-usage--write
     package-usage-file
     (package-usage--merge (package-usage--read package-usage-file)
                           :commands package-usage--pending-commands
                           :modes package-usage--pending-modes
                           :sessions package-usage--pending-sessions
                           :newly-loaded newly-loaded
                           :loaded loaded
                           :now (package-usage--now)))
    (setq package-usage--counted-packages
          (append newly-loaded package-usage--counted-packages))
    (clrhash package-usage--pending-commands)
    (clrhash package-usage--pending-modes)
    (setq package-usage--pending-sessions 0)))

;;;; Report

(defun package-usage--format-time (epoch)
  "Return EPOCH as an ISO 8601 date, or `:null' when EPOCH is nil."
  (if epoch (format-time-string "%Y-%m-%d" epoch) :null))

(defun package-usage--required-by (packages)
  "Return a hash table mapping each name in PACKAGES to its dependents.
PACKAGES is a list of (NAME . REQUIRED-NAMES)."
  (let ((table (make-hash-table :test #'eq)))
    (pcase-dolist (`(,name . ,reqs) packages)
      (dolist (req reqs)
        (when (assq req packages)
          (push name (gethash req table)))))
    table))

(defun package-usage--build-report (data packages resolve now)
  "Return the report for DATA as a plist ready for `json-serialize'.
PACKAGES is a list of (NAME . REQUIRED-NAMES) for every installed
package, RESOLVE maps a function symbol to the package defining it, and
NOW is the report time in seconds since the epoch."
  (let ((usage (make-hash-table :test #'eq))
        (required-by (package-usage--required-by packages)))
    (cl-flet ((add (kind entries)
                (pcase-dolist (`(,symbol ,count . ,last) entries)
                  (when-let* ((pkg (funcall resolve symbol)))
                    (let ((u (or (gethash pkg usage) (list :commands nil :modes 0 :last nil))))
                      (if (eq kind :modes)
                          (plist-put u :modes (+ (plist-get u :modes) count))
                        (plist-put u :commands (cons (cons symbol count) (plist-get u :commands))))
                      (plist-put u :last (max last (or (plist-get u :last) 0)))
                      (puthash pkg u usage))))))
      (add :commands (plist-get data :commands))
      (add :modes (plist-get data :modes)))
    (let ((rows
           (mapcar
            (lambda (package)
              (let* ((name (car package))
                     (u (gethash name usage))
                     (commands (sort (copy-sequence (plist-get u :commands))
                                     (lambda (a b) (> (cdr a) (cdr b)))))
                     (loaded (assq name (plist-get data :loaded))))
                (list :name (symbol-name name)
                      :command_count (apply #'+ (mapcar #'cdr commands))
                      :mode_count (or (plist-get u :modes) 0)
                      :loaded_sessions (if loaded (cadr loaded) 0)
                      :last_used (package-usage--format-time (plist-get u :last))
                      :last_loaded (package-usage--format-time (cddr loaded))
                      :top_commands
                      (vconcat
                       (mapcar (lambda (c) (list :command (symbol-name (car c)) :count (cdr c)))
                               (take package-usage-top-commands commands)))
                      :required_by
                      (vconcat (sort (mapcar #'symbol-name (gethash name required-by))
                                     #'string<)))))
            packages)))
      (list :generated_at (format-time-string "%FT%T%z" now)
            :tracking_since (package-usage--format-time (plist-get data :since))
            :sessions (or (plist-get data :sessions) 0)
            :notes package-usage-report-notes
            :packages
            (vconcat
             (sort rows
                   (lambda (a b)
                     (let ((ua (+ (plist-get a :command_count) (plist-get a :mode_count)))
                           (ub (+ (plist-get b :command_count) (plist-get b :mode_count))))
                       (cond ((/= ua ub) (< ua ub))
                             ((/= (plist-get a :loaded_sessions) (plist-get b :loaded_sessions))
                              (< (plist-get a :loaded_sessions) (plist-get b :loaded_sessions)))
                             (t (string< (plist-get a :name) (plist-get b :name))))))))))))

(defun package-usage--installed-packages ()
  "Return a list of (NAME . REQUIRED-NAMES) for the installed packages."
  (cl-loop for (name desc) in package-alist
           collect (cons name (mapcar #'car (package-desc-reqs desc)))))

;;;###autoload
(defun package-usage-write-report (&optional file)
  "Save the pending counts and write the JSON report to FILE.
FILE defaults to `package-usage-report-file'.  Return the file name."
  (interactive)
  (package-usage-save)
  (let* ((file (or file package-usage-report-file))
         (dirs (package-usage--package-dirs))
         (report (package-usage--build-report
                  (package-usage--read package-usage-file)
                  (package-usage--installed-packages)
                  (lambda (symbol) (package-usage--symbol-package symbol dirs))
                  (package-usage--now))))
    (with-temp-file file
      (insert (json-serialize report))
      (json-pretty-print-buffer)
      (insert "\n"))
    (when (called-interactively-p 'interactive)
      (message "Wrote %s" file))
    file))

;;;; Mode

;;;###autoload
(define-minor-mode package-usage-mode
  "Record how often installed packages are used."
  :global t
  (if package-usage-mode
      (progn
        (add-hook 'post-command-hook #'package-usage--record-command)
        (add-hook 'after-change-major-mode-hook #'package-usage--record-mode)
        (add-hook 'kill-emacs-hook #'package-usage-save)
        (unless package-usage--session-counted
          (setq package-usage--session-counted t)
          (setq package-usage--pending-sessions 1))
        (unless package-usage--timer
          (setq package-usage--timer
                (run-with-timer package-usage-save-interval
                                package-usage-save-interval
                                #'package-usage-save))))
    (remove-hook 'post-command-hook #'package-usage--record-command)
    (remove-hook 'after-change-major-mode-hook #'package-usage--record-mode)
    (remove-hook 'kill-emacs-hook #'package-usage-save)
    (when package-usage--timer
      (cancel-timer package-usage--timer)
      (setq package-usage--timer nil))
    (package-usage-save)))

(provide 'package-usage)
;;; package-usage.el ends here
