;;; package-usage-tests.el --- Tests for package-usage -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "package-usage.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(defun package-usage-tests--table (&rest pairs)
  "Return a hash table built from PAIRS of KEY and (COUNT . LAST)."
  (let ((table (make-hash-table :test #'eq)))
    (while pairs
      (puthash (pop pairs) (pop pairs) table))
    table))

(ert-deftest package-usage-file-package-finds-the-enclosing-package ()
  (let ((dirs '(("/elpa/magit-1/" . magit) ("/elpa/dash-2/" . dash))))
    (should (eq 'magit (package-usage--file-package "/elpa/magit-1/magit-status.elc" dirs)))
    (should (eq 'dash (package-usage--file-package "/elpa/dash-2/dash.el" dirs)))))

(ert-deftest package-usage-file-package-ignores-files-outside-packages ()
  (let ((dirs '(("/elpa/magit-1/" . magit))))
    (should (null (package-usage--file-package "/elpa/magit-10/magit.el" dirs)))
    (should (null (package-usage--file-package "/usr/share/emacs/lisp/simple.elc" dirs)))
    (should (null (package-usage--file-package nil dirs)))))

(ert-deftest package-usage-merge-adds-pending-counts-to-stored-ones ()
  (let ((data (package-usage--merge
               '(:since 100 :sessions 2
                 :commands ((magit-status 3 . 150))
                 :modes ((nix-mode 1 . 120))
                 :loaded ((magit 2 . 150)))
               :commands (package-usage-tests--table
                          'magit-status '(2 . 200) 'avy-goto-char '(1 . 210))
               :modes (package-usage-tests--table)
               :sessions 1
               :newly-loaded '(magit avy)
               :loaded '(magit avy)
               :now 300)))
    (should (= 100 (plist-get data :since)))
    (should (= 3 (plist-get data :sessions)))
    (should (equal '(5 . 200) (alist-get 'magit-status (plist-get data :commands))))
    (should (equal '(1 . 210) (alist-get 'avy-goto-char (plist-get data :commands))))
    (should (equal '(1 . 120) (alist-get 'nix-mode (plist-get data :modes))))
    (should (equal '(3 . 300) (alist-get 'magit (plist-get data :loaded))))
    (should (equal '(1 . 300) (alist-get 'avy (plist-get data :loaded))))))

(ert-deftest package-usage-merge-counts-a-package-once-per-session ()
  (let ((data (package-usage--merge
               '(:since 100 :sessions 1 :loaded ((magit 1 . 150)))
               :commands (package-usage-tests--table)
               :modes (package-usage-tests--table)
               :sessions 0
               :newly-loaded nil
               :loaded '(magit)
               :now 300)))
    (should (= 1 (plist-get data :sessions)))
    (should (equal '(1 . 300) (alist-get 'magit (plist-get data :loaded))))))

(ert-deftest package-usage-merge-starts-from-empty-data ()
  (let ((data (package-usage--merge
               nil
               :commands (package-usage-tests--table 'magit-status '(1 . 50))
               :modes (package-usage-tests--table)
               :sessions 1 :newly-loaded nil :loaded nil :now 60)))
    (should (= 60 (plist-get data :since)))
    (should (= 1 (plist-get data :sessions)))
    (should (equal '(1 . 50) (alist-get 'magit-status (plist-get data :commands))))))

(ert-deftest package-usage-read-returns-what-write-stored ()
  (let ((file (make-temp-file "package-usage" nil ".eld"))
        (data '(:version 1 :since 1 :sessions 2 :commands ((foo 3 . 4)))))
    (unwind-protect
        (progn
          (package-usage--write file data)
          (should (equal data (package-usage--read file))))
      (delete-file file))))

(ert-deftest package-usage-read-returns-nil-for-missing-or-empty-files ()
  (let ((file (make-temp-file "package-usage" nil ".eld")))
    (unwind-protect
        (should (null (package-usage--read file)))
      (delete-file file))
    (should (null (package-usage--read file)))))

(defun package-usage-tests--report ()
  "Build a report over a fixed set of packages and counts."
  (package-usage--build-report
   '(:since 0 :sessions 4
     :commands ((magit-status 5 . 86400) (magit-log 2 . 172800)
                (avy-goto-char 1 . 0) (forward-char 100 . 0))
     :modes ((nix-mode 3 . 0))
     :loaded ((magit 4 . 172800) (transient 4 . 172800) (nix-mode 2 . 0)))
   '((magit transient) (transient) (nix-mode) (avy) (unused))
   (lambda (symbol)
     (pcase symbol
       ((or 'magit-status 'magit-log) 'magit)
       ('avy-goto-char 'avy)
       ('nix-mode 'nix-mode)))
   0))

(defun package-usage-tests--row (report name)
  "Return the row for package NAME in REPORT."
  (cl-find name (plist-get report :packages)
           :key (lambda (row) (plist-get row :name)) :test #'equal))

(ert-deftest package-usage-build-report-aggregates-usage-per-package ()
  (let* ((report (package-usage-tests--report))
         (magit (package-usage-tests--row report "magit")))
    (should (= 4 (plist-get report :sessions)))
    (should (= 7 (plist-get magit :command_count)))
    (should (= 4 (plist-get magit :loaded_sessions)))
    (should (equal (format-time-string "%Y-%m-%d" 172800) (plist-get magit :last_used)))
    (should (equal [(:command "magit-status" :count 5) (:command "magit-log" :count 2)]
                   (plist-get magit :top_commands)))
    (should (= 3 (plist-get (package-usage-tests--row report "nix-mode") :mode_count)))
    (should (equal ["magit"] (plist-get (package-usage-tests--row report "transient")
                                        :required_by)))))

(ert-deftest package-usage-build-report-lists-unused-packages-first ()
  (let* ((report (package-usage-tests--report))
         (names (mapcar (lambda (row) (plist-get row :name)) (plist-get report :packages)))
         (unused (package-usage-tests--row report "unused")))
    (should (equal '("unused" "transient" "avy" "nix-mode" "magit") names))
    (should (= 0 (plist-get unused :command_count)))
    (should (eq :null (plist-get unused :last_used)))
    (should (equal [] (plist-get unused :required_by)))))

(ert-deftest package-usage-build-report-serializes-to-json ()
  (should (stringp (json-serialize (package-usage-tests--report)))))
