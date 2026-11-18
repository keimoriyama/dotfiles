;;; rsync-mode-tests.el --- Tests for local rsync-mode -*- lexical-binding: t; -*-

(require 'ert)

(load (expand-file-name "rsync-mode/rsync-mode.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest rsync-mode-sentinel-ignores-a-deleted-source-buffer ()
  "A process may finish after its Magit or file buffer has been killed."
  (let* ((source (generate-new-buffer " *rsync-deleted-source*"))
         (sentinel (rsync--make-process-sentinel source)))
    (kill-buffer source)
    (funcall sentinel nil "finished\n")
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

(provide 'rsync-mode-tests)
;;; rsync-mode-tests.el ends here
