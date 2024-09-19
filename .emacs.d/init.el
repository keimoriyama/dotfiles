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


;; システムに装飾キー渡さない
(setq mac-pass-control-to-system nil)
(setq mac-pass-command-to-system nil)
(setq mac-pass-option-to-system nil)

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

(with-eval-after-load 'simple
  (setq kill-whole-line t))


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
(tab-bar-mode +1)

(tab-bar-history-mode +1)
(setq tab-bar-show t)
(setq tab-bar-new-button-show nil)
(setq tab-bar-close-button-show nil)

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

(leaf color-moccur
  :bind (("M-o" . occur-by-moccur))
  :custom
  ((dmoccur-exclusion-mask . "\\.DS_Store")(dmoccur-exclusion-mask . "^#.+#$"))
  )

(leaf dash
  :ensure t)
(defadvice moccur-edit-change-file
    (after save-after-moccur-edit-buffer activate)
  (save-buffer))

(leaf kanagawa-theme
  :ensure t
  :config
  (load-theme 'kanagawa t))

(leaf undohist
  :config
  (undohist-initialize))

(cua-mode t)
(setq cua-enable-cua-keys nil)

(leaf projectile
  :ensure t
  :config projectile-mode
  :bind (("C-c p" . projectile-command-map)))

(projectile-mode)

(leaf git-gutter
  :ensure t
  :init
  (global-git-gutter-mode))

(leaf rainbow-delimiters
  :ensure t
  :hook
  ((prog-mode-hook . rainbow-delimiters-mode)))

(leaf hl-line
  :init
  (global-hl-line-mode +1))

(leaf elec-pair
  :config
  (electric-pair-mode +1))

(leaf iflipb
  :ensure t
  :bind
  (("M-n" . iflipb-next-buffer)
   ("M-p" . iflipb-previous-buffer)))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :global-minor-mode global-auto-revert-mode)

(leaf simple
  :doc "basic editing commands for Emacs"
  :custom ((kill-read-only-ok . t)
           (kill-whole-line . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil)))

(leaf spaceline
  :ensure t
  :config (spaceline-spacemacs-theme))


(leaf startup
  :doc "process Emacs shell arguments"
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

(leaf which-key
  :doc "Display available keybindings in popup"
  :ensure t
  :global-minor-mode t)

(leaf magit
  :ensure t)

(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :ensure t
  :defun (exec-path-from-shell-initialize)
  :custom ((exec-path-from-shell-check-startup-files)
           (exec-path-from-shell-variables . '("PATH" "GOPATH" "JAVA_HOME")))
  :config
  (exec-path-from-shell-initialize))

(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :ensure t
  :global-minor-mode t)
 
(leaf marginalia
  :doc "Enrich existing commands with completion annotations"
  :ensure t
  :global-minor-mode t)
 
(leaf consult
  :doc "Consulting completing-read"
  :ensure t
  :hook (completion-list-mode-hook . consult-preview-at-point-mode)
  :defun consult-line
  :preface
  (defun c/consult-line (&optional at-point)
    "Consult-line uses things-at-point if set C-u prefix."
    (interactive "P")
    (if at-point
        (consult-line (thing-at-point 'symbol))
      (consult-line)))
  :custom ((xref-show-xrefs-function . #'consult-xref)
           (xref-show-definitions-function . #'consult-xref)
           (consult-line-start-from-top . t))
  :bind (;; C-c bindings (mode-specific-map)
         ([remap switch-to-buffer] . consult-buffer) ; C-x b
         ([remap project-switch-to-buffer] . consult-project-buffer) ; C-x p b
 
         ;; M-g bindings (goto-map)
         ([remap goto-line] . consult-goto-line)    ; M-g g
         ([remap imenu] . consult-imenu)            ; M-g i
         ("M-g f" . consult-flymake) 
 
         ;; C-M-s bindings
         ("C-s" . c/consult-line)       ; isearch-forward
         ("C-M-s" . nil)                ; isearch-forward-regexp
         ("C-M-s s" . isearch-forward)
         ("C-M-s C-s" . isearch-forward-regexp)
         ("C-M-s r" . consult-ripgrep)
 
         (minibuffer-local-map
          :package emacs
          ("C-r" . consult-history))))

(leaf affe
  :doc "Asynchronous Fuzzy Finder for Emacs"
  :ensure t
  :custom ((affe-highlight-function . 'orderless-highlight-matches)
           (affe-regexp-function . 'orderless-pattern-compiler))
  :bind (("M-s r" . affe-grep)
         ("M-s f" . affe-find)))

(leaf orderless
  :doc "Completion style for matching regexps in any order"
  :ensure t
  :custom ((completion-styles . '(orderless))
           (completion-category-defaults . nil)
           (completion-category-overrides . '((file (styles partial-completion))))))

(leaf embark-consult
  :doc "Consult integration for Embark"
  :ensure t
  :bind ((minibuffer-mode-map
     

     :package emacs
          ("M-." . embark-dwim)
          ("C-." . embark-act))))

(leaf tempel
  :ensure t
  :doc "template engine"
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (add-hook 'org-mode-hook 'tempel-setup-capf))

(leaf yasnippet
  :ensure t
  :doc "snippet engine"
  :init
  (yas-global-mode +1)
  :bind ((
         yas-keymap
         ("<tab>" . nil)
         ("TAB" . nil)
         ("<backtab>" . nil)
         ("S-TAB" . nil)
         ("M-}" . yas-next-field-or-maybe-expand)
         ("M-{" . yas-prev-field))))

(leaf corfu
  :doc "COmpletion in Region FUnction"
  :ensure t
  :global-minor-mode global-corfu-mode corfu-popupinfo-mode
  :custom ((corfu-auto . t)
           (corfu-auto-delay . 0)
           (corfu-cycle . t)
           (corfu-auto-prefix . 1))
  :bind ((corfu-map
          ("C-s" . corfu-insert-separator))))

(leaf corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode))


(leaf cape
  :doc "Completion At Point Extensions"
  :ensure t
  :hook
  ((prog-mode
     text-mode
     conf-mode
     eglot-managed-mode
     lsp-completion-mode
     yatex-mode))
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'tempel-complete)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  )


(leaf treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install t)
  (setq treesit-language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (lua "https://github.com/Azganoth/tree-sitter-lua")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (r "https://github.com/r-lib/tree-sitter-r")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  )

(leaf treesit
  :config
  (setq treesit-font-lock-level 4))


(leaf ddskk
:doc "japanese IME works in emacs"
:bind (("C-x C-j" . skk-mode))
:init
 (setq skk-server-host "localhost")
 (setq skk-server-portnum 1178)
 (setq skk-use-azik t)
 (setq skk-search-katakana t))

 
(leaf org
  :init
  (setq org-directory "~/Documents/org-mode/"
        org-daily-tasks-file (format "%s/tasks.org" org-directory)
        org-memo-file (format "%s/memo.org" org-directory)
        org-main-file (format "%s/main.org" org-directory))
  (setq org-agenda-files (list org-directory))
  (defun my:org-goto-inbox ()
    (interactive)
    (find-file org-main-file))
  :bind
  (("C-c e" . create-daily-org-file)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c i" . my:org-goto-inbox))
  :config
  (setq org-capture-templates
    '(("m" "memo" entry (file org-memo-file)
           "* %U\n%?\n%i\n"
           :empty-lines 1)
      ("t" "Tasks" entry (file+headline org-main-file "inbox") "** TODO %?\n CREATED: %U\n")))
  (setq org-startup-folded nil)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)"))))

(leaf org-agenda
  :commands org-agenda
  :config
  (setq org-agenda-custom-commands
        '(("x" "Unscheduled Tasks" tags-todo
           "-SCHEDULED>=\"<today>\"-DEADLINE>=\"<today>\"" nil)
          ("d" "Daily Tasks" agenda ""
           ((org-agenda-span 1)))))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-return-follows-link t)  ;; RET to follow link
  (setq org-agenda-columns-add-appointments-to-effort-sum t)
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (0900 1200 1300 1800) "......" "----------------"))
  (setq org-columns-default-format
        "%68ITEM(Task) %6Effort(Effort){:} %6CLOCKSUM(Clock){:}")
  (defadvice org-agenda-switch-to (after org-agenda-close)
    "Close a org-agenda window when RET is hit on the window."
    (progn (delete-other-windows)
           (recenter-top-bottom)))
  (ad-activate 'org-agenda-switch-to)
  :bind
  (org-agenda-mode-map
        ("s" . org-agenda-schedule)
        ("S" . org-save-all-org-buffers)))

(leaf flymake
  :ensure t
  :hook ((prog-mode
          conf-mode). flymake-mode))

(leaf flymake-collection
  :ensure t
  :hook ((after-init . flymake-collections-hook-setup)
         ((eglot-managed-mode . (lambda () (add-to-list 'flymake-diagnostic-functions #'eglot-flymake-backend))))))

(add-hook 'python-mode-hook #'python-ts-mode)

(leaf eglot
  :doc "The Emacs Client for LSP servers"
  :custom ((eldoc-echo-area-use-multiline-p . nil)
           (eglot-connect-timeout . 600))
  :config
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'tempel-complete
                       #'eglot-completion-at-point)
                      #'cape-keyword
                      #'cape-dabbrev
                      #'cape-file)
                ))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf))


(leaf eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :vc ( :url "https://github.com/jdtsmith/eglot-booster")
  :global-minor-mode t)


(leaf py-isort :ensure t)

(leaf flymake-ruff
  :ensure t
  :hook (eglot-managed-mode-hook .(lambda ()
                                    (when (derived-mode-p 'python-mode 'python-ts-mode)
                                      (flymake-ruff-load)))))

(leaf highlight-indent-guides
  :ensure t
  :hook ((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode))

(leaf python
  :custom (python-indent-guess-indent-offset-verbose . nil))


(leaf lsp-pyright
  :ensure t
  :hook ((python-ts-mode-hook . (lambda ()
                          (require 'lsp-pyright)))))

(add-hook 'python-ts-mode-hook 'pet-mode -10)

(leaf pet
  :ensure t
  :hook
  ((python-ts-mode . (lambda ()
              (setq-local python-shell-interpreter (pet-executable-find "python")
                          python-shell-virtualenv-root (pet-virtualenv-root))
              (setq-local lsp-pyright-locate-python python-shell-interpreter
                          lsp-pyright-venv-path python-shell-virtualenv-root)
              (lsp)))))

(add-hook 'python-ts-mode 'eglot-ensure)

(leaf yatex
  :doc "new latex mode"
  :ensure t
  :commands (yatex-mode)
  :mode (("\\.tex$" . yatex-mode)
           ("\\.ltx$" . yatex-mode)
           ("\\.cls$" . yatex-mode)
           ("\\.sty$" . yatex-mode)
           ("\\.clo$" . yatex-mode)
           ("\\.bbl$" . yatex-mode)
           ("\\.bib$" . yatex-mode))
    :init
    (setq YaTeX-inhibit-prefix-letter t)
    (setq YaTeX-dvi2-command-ext-alist
    '(("Skim" . ".pdf")))
  (setq dvi2-command "open -a Skim")
  (setq tex-pdfview-command "open -a Skim"))

(leaf flyspell
  ;; flyspellをインストールする
  :ensure t
  ;; YaTeXモードでflyspellを使う
  :hook (yatex-mode-hook . flyspell-mode))

(leaf reftex
    :ensure t
    :after yatex
    :hook (yatex-mode-hook . reftex-mode)
    :bind (reftex-mode-map
                ("C-c (" . reftex-reference)
                ("C-c )" . nil)
                ("C-c >" . YaTeX-comment-region)
                ("C-c <" . YaTeX-uncomment-region))
    :config
    (print (projectile-project-root))
    (setq reftex-default-bibliography (directory-files-recursively
                                       (projectile-project-root) "\\.bib$")))

(leaf ispell
    :ensure t
    :after yatex
    :init
    (setq ispell-local-dictionary "en_US")
    ;; スペルチェッカとしてaspellを使う
    (setq ispell-program-name "aspell")
    :config
    ;; 日本語の部分を飛ばす
    (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
