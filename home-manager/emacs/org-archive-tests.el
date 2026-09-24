;;; org-archive-tests.el --- Tests for my/org-archive-* functions -*- lexical-binding: t; -*-

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

(defmacro org-archive-tests--with-org-buffer (contents &rest body)
  "org-mode の一時バッファに CONTENTS を挿入して BODY を実行する。"
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     ,@body))

(defun org-archive-tests--headings-at-level (level)
  "現在のバッファにある level LEVEL の見出し名をバッファ順に並べたリストを返す。"
  (let (headings)
    (org-map-entries
     (lambda () (push (org-get-heading t t) headings))
     (format "LEVEL=%d" level))
    (nreverse headings)))

(ert-deftest org-archive-done-level-1-moves-done-into-archive-sibling ()
  "DONE の level 1 見出しが archive タグの兄弟見出しに移され、それ以外は残る。"
  (org-archive-tests--with-org-buffer
      "* TODO task\n* DONE done a\nbody a\n* plain heading\n* DONE done b\nbody b\n"
    (my/org-archive-done-level-1)
    (should (equal (org-archive-tests--headings-at-level 1)
                   '("task" "plain heading" "Archive")))
    (should (equal (sort (copy-sequence
                          (org-archive-tests--headings-at-level 2))
                         #'string<)
                   '("done a" "done b")))
    (goto-char (point-min))
    (should (re-search-forward "^\\* Archive\\s-*:ARCHIVE:" nil t))))

(ert-deftest org-archive-done-level-1-keeps-nested-done ()
  "level 2 以下の DONE 見出しは移動しない。"
  (org-archive-tests--with-org-buffer "* parent\n** DONE child\n"
    (my/org-archive-done-level-1)
    (should (equal (buffer-string) "* parent\n** DONE child\n"))))

(ert-deftest org-archive-done-level-1-errors-outside-org-mode ()
  "org-mode 以外のバッファでは user-error。"
  (with-temp-buffer
    (should-error (my/org-archive-done-level-1) :type 'user-error)))

(provide 'org-archive-tests)
;;; org-archive-tests.el ends here
