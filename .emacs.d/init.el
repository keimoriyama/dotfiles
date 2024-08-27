;;; load_pathの追加
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	   (expand-file-name(concat user-emacs-directory path))))
      (add-to-list 'load-path default-directory)
      (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	  (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path "elisp" "conf" "public_repos" "elpa")

(setq custom-file (locate-user-emacs-file "custom.el"))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(define-key global-map (kbd "C-m") 'newline-and-indent)

(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

(define-key global-map (kbd "C-t") 'other-window)

(column-number-mode t)
(size-indication-mode t)
(setq display-time-24hr-format t)
(display-time-mode t)

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

(setq frame-title-format "%f")

(global-display-line-numbers-mode 1)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 200)
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match nil)
(set-face-underline 'show-paren-match "darkgreen")

(setq backup-directory-alist
      `((".*".,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory)))

(global-auto-revert-mode t)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
(setq mac-command-modifier 'meta)
(defun elisp-mode-hooks ()
  "list-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

(leaf helm)
;;
(leaf auto-complete-config
  :config
  (ac-config-default)
  :bind (("M-TAB" . auto-complete))
  :custom
  ((ac-use-menu-map . t)(ac-ignore-case . nil))
  )
(leaf color-moccur
  :bind (("M-o" . occur-by-moccur))
  :custom
  ((dmoccur-exclusion-mask . "\\.DS_Store")(dmoccur-exclusion-mask . "^#.+#$"))
  )
;(require 'moccur-edit nil t)


(defadvice moccur-edit-change-file
    (after save-after-moccur-edit-buffer activate)
  (save-buffer))

(leaf wgrep)
(leaf undohist
  :config
  (undohist-initialize))
(leaf elscreen
  :config
  (elscreen-start)
  :if
  (window-system)
  :bind (("C-z" . iconify-or-deiconify-frame)("C-z" . suspend-emacs)))
;(when (require 'elscreen nil t)
;  (elscreen-start)
;  (if window-system
;      (define-key elscreen-map (kbd "C-z")
;                  'iconify-or-deinconify-frame)
;    (define-key elscreen-map (kbd "C-z")
;                'suspend-emacs)))

(cua-mode t)
(setq cua-enable-cua-keys nil)


(setq python-check-command "black")
 
(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck(flycheck-pos-tip-mode))

(leaf projectile
  :config projectile-mode)
(leaf git-gutter
  :custom
  (global-git-gutter-mode . t))

;; 画像をインラインで表示
(setq org-startup-with-inline-images t)
 
;; 見出しの余分な*を消す
(setq org-hide-leading-stars t)
 
;; LOGBOOK drawerに時間を格納する
(setq org-clock-into-drawer t)
 
;; .orgファイルは自動的にorg-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
 
;; org-directory内のファイルすべてからagendaを作成する
(setq my-org-agenda-dir "~/org/")
(setq org-agenda-files (list my-org-agenda-dir))
 
;; TODO状態
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "NOTE(n)"  "|" "DONE(d)" "SOMEDAY(s)" "CANCEL(c)")))
 
;; DONEの時刻を記録
(setq org-log-done 'time)
 
;; ショートカットキー
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
