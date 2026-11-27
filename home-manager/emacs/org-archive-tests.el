;;; org-archive-tests.el --- Tests for my/org-archive-* functions -*- lexical-binding: t; -*-

;; my/org-archive-* は init.el (init.org から tangle) で定義される。
;; init.el の再生成を待たずに作業ツリーの定義を検証するため、
;; init.org から該当するソースブロックだけを評価してから使う。

(require 'ert)
(require 'org)
(require 'org-archive)

;; init.el では setq でしか設定していないので、ここで special 宣言して
;; let による動的再束縛を有効にする
(defvar org-prj-dir)
(defvar org-archive-dir)

(with-temp-buffer
  (insert-file-contents
   (expand-file-name "init.org"
                     (file-name-directory
                      (or load-file-name buffer-file-name))))
  (delay-mode-hooks (org-mode))
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*#\\+begin_src emacs-lisp" nil t)
    (let ((body (org-element-property :value (org-element-at-point))))
      (when (string-match-p "^(defun my/org-archive-" body)
        (eval (car (read-from-string (concat "(progn\n" body "\n)"))) t)))))

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

(ert-deftest org-archive-project-file-rejects-directory ()
  "projects 内でもディレクトリは user-error で拒否し、移動されない。"
  (org-archive-tests--with-dirs
    (let ((dir (expand-file-name "sub" org-prj-dir)))
      (make-directory dir)
      (should-error (my/org-archive-project-file dir) :type 'user-error)
      (should-error (my/org-archive-project-file (file-name-as-directory dir))
                    :type 'user-error)
      (should (file-directory-p dir))
      (should-not (file-exists-p org-archive-dir)))))

(ert-deftest org-archive-project-file-follows-visiting-buffer ()
  "開いているバッファは移動先のファイルを visit するようになる。"
  (org-archive-tests--with-dirs
    (let* ((src (expand-file-name "open.org" org-prj-dir))
           (dest (expand-file-name "open.org" org-archive-dir))
           (buf (progn (write-region "* a\n" nil src nil 'silent)
                       (find-file-noselect src))))
      (unwind-protect
          (progn
            (my/org-archive-project-file src)
            (should (equal (buffer-file-name buf) dest))
            (should-not (buffer-modified-p buf)))
        (kill-buffer buf)))))

(ert-deftest org-archive-project-file-refuses-unsaved-buffer ()
  "未保存の変更があるバッファのファイルは user-error で移動しない。"
  (org-archive-tests--with-dirs
    (let* ((src (expand-file-name "dirty.org" org-prj-dir))
           (buf (progn (write-region "* a\n" nil src nil 'silent)
                       (find-file-noselect src))))
      (unwind-protect
          (progn
            (with-current-buffer buf (insert "edit"))
            (should-error (my/org-archive-project-file src) :type 'user-error)
            (should (file-exists-p src)))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)))))

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

(ert-deftest org-archive-done-level-1-uses-existing-archive-sibling-above ()
  "既存の archive 見出しが DONE 見出しより前にあっても、全 DONE がそこへ入る。"
  (org-archive-tests--with-org-buffer
      "* Archive :ARCHIVE:\n** DONE old\n* DONE a\nbody a\n* DONE b\n* TODO c\n"
    (my/org-archive-done-level-1)
    (should (equal (org-archive-tests--headings-at-level 1)
                   '("Archive" "c")))
    (should (equal (sort (copy-sequence
                          (org-archive-tests--headings-at-level 2))
                         #'string<)
                   '("a" "b" "old")))
    (should-not (org-archive-tests--headings-at-level 3))
    (goto-char (point-min))
    (should (search-forward "body a" nil t))))

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
