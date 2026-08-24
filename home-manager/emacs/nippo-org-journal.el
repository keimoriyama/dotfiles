;;; nippo-org-journal.el --- Sync nippo reports to Org Journal -*- lexical-binding: t; -*-

(require 'org)
(require 'org-journal)
(require 'project)
(require 'subr-x)

;;;###autoload
(defun nippo-org-journal-sync ()
  "Convert today's nippo report and replace its Org Journal section."
  (interactive)
  (let* ((project (project-current nil))
         (project-root (and project (project-root project)))
         (report-name (format "nippo-%s.md" (format-time-string "%Y-%m-%d")))
         (report (and project-root
                      (expand-file-name (concat "reports/" report-name)
                                        project-root))))
    (unless project-root
      (user-error "Current directory does not belong to a project"))
    (unless (file-readable-p report)
      (user-error "Nippo report not found: %s" report))
    (unless (executable-find "pandoc")
      (user-error "Pandoc is required to convert the nippo report"))
    (let ((converted
           (with-temp-buffer
             (insert-file-contents report)
             (let ((status
                    (call-process-region
                     (point-min) (point-max) "pandoc" t '(t nil) nil
                     "--from=gfm" "--to=org" "--shift-heading-level-by=2")))
               (unless (and (integerp status) (zerop status))
                 (user-error "Pandoc failed to convert %s" report)))
             (string-trim-right (buffer-string))))
          journal-file)
      (save-window-excursion
        (org-journal-new-entry t)
        (setq journal-file (buffer-file-name))
        (save-restriction
          (widen)
          (goto-char (point-min))
          (if (re-search-forward "^\\*\\* Nippo[ \t]*$" nil t)
              (progn
                (beginning-of-line)
                (let ((start (point)))
                  (org-end-of-subtree t t)
                  (delete-region start (point))))
            (goto-char (point-max)))
          (unless (bolp)
            (insert "\n"))
          (insert "** Nippo\n"
                  "- 元レポート :: "
                  (org-link-make-string (concat "file:" report) report-name)
                  "\n\n"
                  converted
                  "\n")
          (save-buffer)))
      (message "Synced %s to %s" report-name journal-file)
      journal-file)))

(provide 'nippo-org-journal)
;;; nippo-org-journal.el ends here
