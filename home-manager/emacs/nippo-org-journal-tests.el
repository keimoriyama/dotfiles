;;; nippo-org-journal-tests.el --- Tests for nippo Org Journal sync -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(load (expand-file-name "nippo-org-journal.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest nippo-org-journal-sync-replaces-todays-section-idempotently ()
  "Today's converted report replaces the existing Nippo section exactly once."
  (let* ((project-root (make-temp-file "nippo-project-" t))
         (reports-dir (expand-file-name "reports" project-root))
         (report-name (format "nippo-%s.md" (format-time-string "%Y-%m-%d")))
         (report (expand-file-name report-name reports-dir))
         (journal (generate-new-buffer " *nippo-journal-test*")))
    (unwind-protect
        (progn
          (make-directory reports-dir)
          (write-region "# 日報\n\n本文\n" nil report nil 'silent)
          (with-current-buffer journal
            (org-mode)
            (insert "* Today\n** Existing\nKeep\n\n** Nippo\nOld\n"))
          (cl-letf (((symbol-function 'project-current) (lambda (&optional _) 'project))
                    ((symbol-function 'project-root) (lambda (_) project-root))
                    ((symbol-function 'executable-find) (lambda (_) "/bin/pandoc"))
                    ((symbol-function 'call-process-region)
                     (lambda (start end &rest _)
                       (delete-region start end)
                       (insert "*** 日報\n\n本文\n")
                       0))
                    ((symbol-function 'org-journal-new-entry)
                     (lambda (&rest _) (set-buffer journal)))
                    ((symbol-function 'save-buffer) (lambda (&rest _) t)))
            (nippo-org-journal-sync)
            (nippo-org-journal-sync))
          (with-current-buffer journal
            (goto-char (point-min))
            (should (= 1 (how-many "^\\*\\* Nippo$")))
            (should (search-forward "*** 日報" nil t))
            (should (search-forward "本文" nil t))
            (goto-char (point-min))
            (should (search-forward report-name nil t))
            (goto-char (point-min))
            (should (search-forward "** Existing\nKeep" nil t))))
      (kill-buffer journal)
      (delete-directory project-root t))))

(ert-deftest nippo-org-journal-sync-rejects-a-missing-report ()
  "A missing daily report leaves Org Journal unopened."
  (let ((project-root (make-temp-file "nippo-project-" t))
        (journal-opened nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&optional _) 'project))
                  ((symbol-function 'project-root) (lambda (_) project-root))
                  ((symbol-function 'org-journal-new-entry)
                   (lambda (&rest _) (setq journal-opened t))))
          (should-error (nippo-org-journal-sync) :type 'user-error)
          (should-not journal-opened))
      (delete-directory project-root t))))

(provide 'nippo-org-journal-tests)
;;; nippo-org-journal-tests.el ends here
