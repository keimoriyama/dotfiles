;;; org-archive-tests.el --- Tests for my/org-archive-project-file -*- lexical-binding: t; -*-

;; my/org-archive-project-file は init.el (init.org から tangle) で定義される。
;; そのため実行中の Emacs (init.el 読み込み済み) への emacsclient 経由で動かす前提。

(require 'ert)

;; init.el では setq でしか設定していないので、ここで special 宣言して
;; let による動的再束縛を有効にする
(defvar org-prj-dir)
(defvar org-archive-dir)

(defmacro org-archive-tests--with-dirs (&rest body)
  "projects/ を一時ディレクトリに作り、archive/ はコマンド側に作らせる形で BODY を実行する。"
  (declare (indent 0))
  (let ((root (make-symbol "root")))
    `(let ((,root (make-temp-file "org-archive-" t)))
       (unwind-protect
           (let ((org-prj-dir (expand-file-name "projects" ,root))
                 (org-archive-dir (expand-file-name "archive" ,root)))
             (make-directory org-prj-dir t)
             ,@body)
         (delete-directory ,root t)))))

(ert-deftest org-archive-project-file-moves-file-to-archive ()
  "projects 内のファイルを archive へ移動できる。"
  (org-archive-tests--with-dirs
    (let ((src (expand-file-name "done.org" org-prj-dir)))
      (write-region "* DONE task\n" nil src nil 'silent)
      (my/org-archive-project-file src)
      (should-not (file-exists-p src))
      (should (file-exists-p (expand-file-name "done.org" org-archive-dir))))))

(ert-deftest org-archive-project-file-rejects-file-outside-projects ()
  "projects 以外のファイルは user-error で拒否し、移動されない。"
  (org-archive-tests--with-dirs
    (let ((outside (expand-file-name
                    "memo.org" (file-name-directory org-prj-dir))))
      (write-region "* memo\n" nil outside nil 'silent)
      (should-error (my/org-archive-project-file outside) :type 'user-error)
      (should (file-exists-p outside)))))

(ert-deftest org-archive-project-file-does-not-overwrite-existing-dest ()
  "archive に同名ファイルがある場合は user-error で既存ファイルを上書きしない。"
  (org-archive-tests--with-dirs
    (make-directory org-archive-dir t)
    (let ((src (expand-file-name "dup.org" org-prj-dir))
          (dest (expand-file-name "dup.org" org-archive-dir)))
      (write-region "* new\n" nil src nil 'silent)
      (write-region "* old\n" nil dest nil 'silent)
      (should-error (my/org-archive-project-file src) :type 'user-error)
      (should (file-exists-p src))
      (should (string= "* old\n"
                       (with-temp-buffer
                         (insert-file-contents dest)
                         (buffer-string)))))))

(provide 'org-archive-tests)
;;; org-archive-tests.el ends here
