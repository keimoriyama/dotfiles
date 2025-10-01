;;; init.el:
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

; システムに装飾キー渡さない
(setq mac-pass-control-to-system nil)
(setq mac-pass-command-to-system nil)
(setq mac-pass-option-to-system nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)

(define-key global-map (kbd "C-t") 'other-window)

(global-set-key (kbd "C-x C-c") 'magit)
(global-set-key (kbd "C-x C-z") 'your-favorite-command)
;; I never use C-x C-c
(defalias 'exit 'save-buffers-kill-emacs)

(server-start)
(column-number-mode -1)
(scroll-bar-mode -1)
(size-indication-mode t)
(setq display-time-24hr-format t)
(display-time-mode t)

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq frame-title-format "%f")

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 200)

(setq backup-directory-alist
      `((".*".,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory)))

(global-auto-revert-mode t)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(setq mac-command-modifier 'meta)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (fset 'yes-or-no-p 'y-or-n-p)))

(with-eval-after-load 'comp-run
  ;; config
  (setopt native-comp-async-jobs-number 8)
  (setopt native-comp-speed 3)
  (setopt native-comp-always-compile t))

(with-eval-after-load 'warnings
  ;; config
  (setopt warning-suppress-types '((comp))))

(defun elisp-mode-hooks ()
  "list-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

;; load environment value
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))

; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")
                       ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf pretty-hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init))
)

(leaf dash
  :ensure t)

(leaf f
  :ensure t)

(leaf nerd-icons-completion
  :ensure t
  :global-minor-mode t)

(defadvice moccur-edit-change-file
    (after save-after-moccur-edit-buffer activate)
  (save-buffer))

(leaf solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

(leaf volatile-highlights
  :ensure t
  :global-minor-mode t)

(cua-mode t)
(setq cua-enable-cua-keys nil)

(leaf projectile
  :ensure t
  :global-minor-mode projectile-mode
  :custom
  ((projectile-sort-order . 'recently-active))
  :bind (("C-c p" . projectile-command-map)))

;; (leaf centaur-tabs
;;   :ensure t
;;   :global-minor-mode centaur-tabs-mode
;;   :bind ("M-c" . centaur-tabs/body)
;;   :custom
;;   ((centaur-tabs--buffer-show-groups . t)
;;    (centaur-tabs-set-icons . t)
;;    (entaur-tabs-icon-type . 'nerd-icons))
;;   :pretty-hydra
;;   ((:color blue :quit-key "q" :foreign-keys warn)
;;    ("Move Buffer"
;;     (("n" centaur-tabs-forward "forward next" :exit nil)
;;      ("p" centaur-tabs-backward "back forward" :exit nil)
;;      ("g" centaur-tabs-switch-group "move group" :exit nil))
;;     "Kill Buffer"
;;     (("k" centaur-tabs-kill-all-buffers-in-current-group "kill all buffer in group" :exit nil)
;;      ("K" centaur-tabs-kill-other-buffers-in-current-group "kill other buffer in group" :exit nil)))))

(leaf bufferlo
  :ensure t
  :global-minor-mode bufferlo-mode)

(leaf expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(leaf undo-tree
  :ensure t
  :global-minor-mode global-undo-tree-mode)

;; (leaf multiple-cursors
;;   :ensure t
;;   :bind ("M-m" . hydra-multiple-cursors/body)
;;   :hydra
;;   (hydra-multiple-cursors (:color pink :hint nil)
;; "
;;                                                                         ╔════════╗
;;     Point^^^^^^             Misc^^            Insert                            ║ Cursor ║
;;   ──────────────────────────────────────────────────────────────────────╨────────╜
;;      _k_    _K_    _M-k_    [_l_] edit lines  [_i_] 0...
;;      ^↑^    ^↑^     ^↑^     [_m_] mark all    [_a_] letters
;;     mark^^ skip^^^ un-mk^   [_s_] sort
;;      ^↓^    ^↓^     ^↓^
;;      _j_    _J_    _M-j_
;;   ╭──────────────────────────────────────────────────────────────────────────────╯
;;                            [_q_]: quit, [Click]: point
;; "
;;           ("l" mc/edit-lines :exit t)
;;           ("m" mc/mark-all-like-this :exit t)
;;           ("j" mc/mark-next-symbol-like-this)
;;           ("J" mc/skip-to-next-like-this)
;;           ("M-j" mc/unmark-next-like-this)
;;           ("k" mc/mark-previous-symbol-like-this)
;;           ("K" mc/skip-to-previous-like-this)
;;           ("M-k" mc/unmark-previous-like-this)
;;           ("s" mc/mark-all-in-region-regexp :exit t)
;;           ("i" mc/insert-numbers :exit t)
;;           ("a" mc/insert-letters :exit t)
;;           ("<mouse-1>" mc/add-cursor-on-click)
;;           ;; Help with click recognition in this hydra
;;           ("<down-mouse-1>" ignore)
;;           ("<drag-mouse-1>" ignore)
;;           ("q" nil)))

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

(leaf free-keys
  :ensure t)

(leaf puni
  :doc "Parentheses Universalistic"
  :ensure t
  :global-minor-mode puni-global-mode
  :bind ((puni-mode-map
       ;; default mapping
       ;; ("C-M-f" . puni-forward-sexp)
       ;; ("C-M-b" . puni-backward-sexp)
       ;; ("C-M-a" . puni-beginning-of-sexp)
       ;; ("C-M-e" . puni-end-of-sexp)
       ;; ("M-)" . puni-syntactic-forward-punct)
       ;; ("C-M-u" . backward-up-list)
       ;; ("C-M-d" . backward-down-list)
       
       ("C-(" . puni-slurp-forward)
       ("C-}" . puni-barf-forward)
       ("M-(" . puni-wrap-round)
       ("M-s" . puni-splice)
       ("M-r" . puni-raise)
       ("M-U" . puni-splice-killing-backward)
       ("M-z" . puni-squeeze)))
  :config
  (leaf electric-pair-mode
    :global-minor-mode t))

;; (leaf iflipb
;;   :ensure t
;;   :bind
;;   (("M-n" . iflipb-next-buffer)
;;    ("M-p" . iflipb-previous-buffer)))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :global-minor-mode global-auto-revert-mode)

(leaf simple
  :doc "basic editing commands for Emacs"
  :custom ((kill-read-only-ok . t)
           (kill-whole-line . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil)))

(leaf doom-modeline
  :ensure t
  :global-minor-mode doom-modeline-mode)

(leaf startup
  :doc "process Emacs shell arguments")
;  :custom `((auto-save-list-file-prefix . '(locate-user-emacs-file "backup/.saves-"))))

(leaf mistty
  :ensure t
  :bind (("C-c s" . mistty-other-window)

       ;; bind here the shortcuts you'd like the
       ;; shell to handle instead of Emacs.
       (mistty-prompt-map

       ;; fish: directory history
       ("M-<up>" . mistty-send-key)
       ("M-<down>" . mistty-send-key)
       ("M-<left>" . mistty-send-key)
       ("M-<right>" . mistty-send-key))))

(leaf which-key
  :doc "Display available keybindings in popup"
  :ensure t
  :global-minor-mode t)

(leaf magit
  :ensure t)

(leaf smerge-mode
  :doc "Manage git confliction"
  :ensure t
  :preface
  (defun start-smerge-mode-with-hydra ()
    (interactive)
    (progn
      (smerge-mode 1)
      (smerge-mode/body)))
  :pretty-hydra
  ((:color blue :quit-key "q" :foreign-keys warn)
   ("Move"
    (("n" smerge-next "next")
     ("p" smerge-prev "preview"))
    "Keep"
    (("b" smerge-keep-base "base")
     ("u" smerge-keep-upper "upper")
     ("l" smerge-keep-lower "lower")
     ("a" smerge-keep-all "both")
     ("\C-m" smerge-keep-current "current"))
    "Others"
    (("C" smerge-combine-with-next "combine with next")
     ("r" smerge-resolve "resolve")
     ("k" smerge-kill-current "kill current"))
    "End"
    (("ZZ" (lambda ()
             (interactive)
             (save-buffer)
             (bury-buffer))
      "Save and bury buffer" :color blue)
     ("q" nil "cancel" :color blue)))))

(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :ensure t
  :defun (exec-path-from-shell-initialize)
  :custom ((exec-path-from-shell-check-startup-files)
           (exec-path-from-shell-variables . '("PATH" "GOPATH" "JAVA_HOME")))
  :config
  (exec-path-from-shell-initialize))

(leaf corfu
  :doc "COmpletion in Region FUnction"
  :ensure t
  :global-minor-mode global-corfu-mode
  :custom ((corfu-auto . t)
           (corfu-auto-delay . 0.1)
           (corfu-cycle . t)
           (corfu-auto-prefix . 3)
           (text-mode-ispell-word-completion . nil))
  :bind ((corfu-map
          ("C-s" . corfu-insert-separator))))

(leaf cape
  :doc "Completion At Point Extensions"
  :ensure t
  :hook
  ((prog-mode
     text-mode
     conf-mode
     lsp-completion-mode))
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'tempel-complete)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex))

(savehist-mode)
(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :ensure t
  :global-minor-mode t)
(advice-add #'vertico--setup :after
            (lambda (&rest _)
              (setq-local completion-auto-help nil
                          completion-show-inline-help nil)))

(leaf marginalia
  :doc "Enrich existing commands with completion annotations"
  :ensure t
  :global-minor-mode t)

(leaf avy
  :doc "Jump to things in tree-style"
  :url "https://github.com/abo-abo/avy"
  :ensure t)

(leaf avy-zap
  :doc "Zap to char using avy"
  :url "https://github.com/cute-jumper/avy-zap"
  :ensure t)

(defvar my-consult--source-buffer
  `(:name "Other Buffers"
    :narrow   ?b
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :items ,(lambda () (consult--buffer-query
                        :predicate #'bufferlo-non-local-buffer-p
                        :sort 'visibility
                        :as #'buffer-name)))
    "Non-local buffer candidate source for `consult-buffer'.")

(defvar my-consult--source-local-buffer
  `(:name "Local Buffers"
    :narrow   ?l
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :default  t
    :items ,(lambda () (consult--buffer-query
                        :predicate #'bufferlo-local-buffer-p
                        :sort 'visibility
                        :as #'buffer-name)))
    "Local buffer candidate source for `consult-buffer'.")

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
           (consult-line-start-from-top . t)
           (consult-buffer-sources . '(consult--source-hidden-buffer
                                       my-consult--source-local-buffer
                                       my-consult--source-buffer)))
  :bind (;; C-c bindings (mode-specific-map)
         ([remap switch-to-buffer] . consult-buffer) ; C-x b
         ([remap project-switch-to-buffer] . consult-project-buffer) ; C-x p b

         ;; M-g bindings (goto-map)
         ([remap goto-line] . consult-goto-line)    ; M-g g
         ([remap imenu] . consult-imenu)            ; M-g i

         (minibuffer-local-map
          :package emacs
          ("C-r" . consult-history))))

(leaf embark
      :ensure t
      :bind
      (("C-h b" . embark-bindings)
       ("C-." . embark-act)
       ("C-;" . embark-dwin)))

(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

(leaf embark-consult
      :doc "Consult integration for Embark"
      :ensure t
      :after (embark consult)
      :hook
      (embark-collect-mode-hook . consult-preview-at-point-mode)
      )

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr
binding))))))))

(setq embark-indicators
  '(embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

(leaf affe
  :doc "Asynchronous Fuzzy Finder for Emacs"
  :ensure t
  :custom ((affe-highlight-function . 'orderless-highlight-matches)
           (affe-regexp-function . 'orderless-pattern-compiler)))

(leaf orderless
  :doc "Completion style for matching regexps in any order"
  :ensure t
  :custom ((completion-styles . '(orderless partial-completion basic))
           (completion-category-defaults . nil)
           (completion-category-overrides . nil)))

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
  )

(leaf yasnippet
  :ensure t
  :doc "snippet engine"
  :init (yas-global-mode t)
  :bind ((yas-keymap
         ("<tab>" . nil)
         ("TAB" . nil)
         ("<backtab>" . nil)
         ("S-TAB" . nil)
         ("M-}" . yas-next-field-or-maybe-expand)
         ("M-{" . yas-prev-field)))
  :bind
  ("C-c y" . yasnippet/body)
  :pretty-hydra
  ((:title "snippet" :color blue :quit-key "q" :foreign-keys warn :separator "╌")
   ("Basic"
    (("a" yas-new-snippet "add new snippet")
     ("i" yas-insert-snippet "insert snippet")
     ("e" yas-visit-snippet-file "edit snippet")))))

(setq org-directory "~/Documents/org-mode"
      org-memo-file (format "%s/memo.org" org-directory)
      org-daily-todo-file (format "%s/daily_todo.org" org-directory)
      org-memo-dir (format "%s/memo/" org-directory))

(defun create-new-org-file (path)
  (let ((name (read-string "Name: ")))
    (expand-file-name (format "%s.org"
                              name) path)))

(leaf org
  :custom
  ((org-startup-folded . 'content)
   (org-startup-indented . "indent")
   (org-deadline-warning-days . 30)
   (org-capture-templates .
    '(("m" "Memo" entry (file org-memo-file) "** %U\n%?\n" :empty-lines 1)
      ("t" "Tasks" entry (file+datetree org-daily-todo-file) "** TODO %?")
      ("p" "Projects" entry (file
                               (lambda () (create-new-org-file
                                     (format "%s/projects/" org-directory))))
       "\n* %? \n** 目的 \n- \n** やること\n*** \n** 結果\n-")))
   (org-todo-keywords .
                      '((sequence "TODO" "DOING" "|"  "DONE" "WAIT")))
   (org-global-properties . '(("EFFORT_ALL" . "25 50 75 90"))))
  )

(leaf olivetti
  :ensure t
  :hook (org-mode-hook . olivetti-mode))

(when (member "Roboto Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Roboto Mono" :height 110)
  (set-face-attribute 'fixed-pitch nil :family "Roboto Mono"))

(when (member "Source Sans Pro" (font-family-list))
  (set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 1.18))
;; Resize Org headings
(dolist (face '((org-level-1 . 1.35)
                (org-level-2 . 1.3)
                (org-level-3 . 1.2)
                (org-level-4 . 1.1)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1))))

;; Make the document title a bit bigger

(require 'org-indent)
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-block nil            :foreground nil :inherit
                    'fixed-pitch :height 1.0)
(set-face-attribute 'org-code nil             :inherit '(shadow fixed-pitch) :height 0.85)
(set-face-attribute 'org-indent nil           :inherit '(org-hide fixed-pitch) :height 0.85)
(set-face-attribute 'org-verbatim nil         :inherit '(shadow fixed-pitch) :height 0.85)
(set-face-attribute 'org-special-keyword nil  :inherit '(font-lock-comment-face
                                                              fixed-pitch))
(set-face-attribute 'org-meta-line nil        :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil         :inherit 'fixed-pitch)
(add-hook 'org-mode-hook 'variable-pitch-mode)
(plist-put org-format-latex-options :scale 2)
(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-pretty-entities t
	  org-ellipsis "  ·")
(setq org-src-fontify-natively t
	  org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)
(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-lowest-priority ?F)  ;; Gives us priorities A through F
(setq org-default-priority ?E) ;; If an item has no priority, it is considered [#E].

(setq org-priority-faces
      '((65 . "#BF616A")
        (66 . "#EBCB8B")
        (67 . "#B48EAD")
        (68 . "#81A1C1")
        (69 . "#5E81AC")
        (70 . "#4C566A")))
; Needs no action currently
(setq org-todo-keyword-faces
      '(("TODO"      :inherit (org-todo region) :foreground "#A3BE8C" :weight bold)
        ("DOING"      :inherit (org-todo region) :foreground "#88C0D0" :weight bold)
		("WAIT"      :inherit (org-todo region) :foreground "#88C0D0" :weight bold)
        ;; ("READ"      :inherit (org-todo region) :foreground "#8FBCBB" :weight bold)
		;; ("CHECK"     :inherit (org-todo region) :foreground "#81A1C1" :weight bold)
		;; ("IDEA"      :inherit (org-todo region) :foreground "#EBCB8B" 
         ;;:weight bold)
		("DONE"      :inherit (org-todo region) :foreground "#30343d" :weight bold)))

(leaf org-superstar
  :hook (org-mode-hook . org-superstar-mode)
  :ensure t
  :custom
  ((org-superstar-leading-bullet . " ")
   (org-superstart-special-todo-items . t)))

(org-babel-do-load-languages
'org-babel-load-languages
'((python . t)
  (shell . t)))

(setq org-babel-python-command "../.venv/bin/python")

(leaf org-agenda
  :commands org-agenda
  :custom
  ((org-agenda-custom-commands .
        '(("x" "Unscheduled Tasks" tags-todo
           "-SCHEDULED>=\"<today>\"-DEADLINE>=\"<today>\"" nil)
          ))
  (org-agenda-start-on-weekday . 3)
  (org-agenda-span . 'week)
  (org-agenda-skip-scheduled-if-done . t)
  (org-return-follows-link . t)  ;; RET to follow link
  (org-agenda-columns-add-appointments-to-effort-sum . t)
  (org-agenda-time-grid .
                        '((daily today require-timed)
                          (0900 1200 1300 1800) "......" "----------------"))
  (org-columns-default-format . 
                              "%68ITEM(Task) %6Effort(Effort){:} %6CLOCKSUM(Clock){:}")
  (org-clock-out-remove-zero-time-clocks . t)
  (org-agenda-use-time-grid . t)
  (org-clock-clocked-in-display          . 'both)
  (org-agenda-start-with-log-mode        . t)
  (org-agenda-files . '("~/Documents/org-mode/projects")))
  :bind
  ((org-agenda-mode-map
        ("s" . org-agenda-schedule)
        ("S" . org-save-all-org-buffers))
   ("C-c C-c" . org-agenda))
  )

(plist-put org-format-latex-options :scale 1.2)

(leaf org-pomodoro
  :ensure t
; :hook ((org-pomodoro-break-finished-hook . org-pomodoro))
  :bind ("M-p" . org-pomodoro)
  :custom ((org-pomodoro-play-sournds . nil)
           (org-pomodoro-finished-sound-p . nil)
           (org-pomodoro-short-break-sound-p . nil)
           (org-pomodoro-long-break-sound-p . nil)
           (org-pomodoro-manual-break . t)
           (org-pomodoro-format . "Working %s")
           (org-pomodoro-length . 50)
           (org-pomodoro-short-break-length . 10)))
(defun org-pomodoro-kill ()
  "Kill the current timer, reset the phase and update the modeline."
  (org-clock-out)
  (org-pomodoro-killed))

(leaf ox-gfm
  :ensure t
  :after org)

(defun my:org-goto-project ()
    (interactive)
    (find-file org-project-file))
(defun my:org-goto-memo ()
    (interactive)
    (find-file org-memo-file))
(defun my:org-goto-exp ()
    (interactive)
    (find-file org-exp-file))
(defun my:org-goto-daily-todo ()
  (interactive)
  (find-file org-daily-todo-file))

(leaf *hydra-org
  :bind ("C-c o". *hydra-org/body)
  :pretty-hydra
  ((:title "org mode":color blue :quit-key "q" :foreign-keys warn :separator "╌")
   ("visit file"
    (("m" my:org-goto-memo "memo")
     ("t" my:org-goto-daily-todo "todo"))
    "agenda"
    (("a" org-agenda "open agenda")
     ("c" org-capture "capture"))
    )
   )
  )

(defun my/lsp-mode-completion ()
   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
         '(orderless)))

(leaf lsp-mode
  :ensure t
  :hook
  ((python-mode-hook . lsp-mode)
   (lsp-completion-mode-hook . my/lsp-mode-completion))
  :custom
  (lsp-enable-file-watchers . nil)
  (lsp-file-watch-threshold . 500)
  (lsp-completion-provider . :none)
  (lsp-ruff-lsp-server-command . '("ruff" "server"))
                                        ;(lsp-use-plists . t)
)

;; (setq read-process-output-max (* 5 1024 1024)) ;; 10mb
;; (setq gc-cons-threshold 200000000)

;; (defun lsp-booster--advice-json-parse (old-fn &rest args)
;;   "Try to parse bytecode instead of json."
;;   (or
;;    (when (equal (following-char) ?#)
;;      (let ((bytecode (read (current-buffer))))
;;        (when (byte-code-function-p bytecode)
;;          (funcall bytecode))))
;;    (apply old-fn args)))
;; (advice-add (if (progn (require 'json)
;;                        (fboundp 'json-parse-buffer))
;;                 'json-parse-buffer
;;               'json-read)
;;             :around
;;             #'lsp-booster--advice-json-parse)

;; (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
;;   "Prepend emacs-lsp-booster command to lsp CMD."
;;   (let ((orig-result (funcall old-fn cmd test?)))
;;     (if (and (not test?)                             ;; for check lsp-server-present?
;;              (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
;;              lsp-use-plists
;;              (not (functionp 'json-rpc-connection))  ;; native json-rpc
;;              (executable-find "emacs-lsp-booster"))
;;         (progn
;;           (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
;;             (setcar orig-result command-from-exec-path))
;;           (message "Using emacs-lsp-booster for %s!" orig-result)
;;           (cons "emacs-lsp-booster" orig-result))
;;       orig-result)))
;; (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(leaf lsp-ui
 :hook (lsp-mode-hook . (lsp-ui-mode lsp-ui-sideline-update-mode))
 :ensure t
 :bind
 ("C-c l" . lsp-ui/body)
 :pretty-hydra
  ((:title "LSP" :color blue :quit-key "q" :foreign-keys warn :separator "╌")
   ("peek"
    (("d" lsp-ui-peek-find-definitions "definitions")
     ("r" lsp-ui-peek-find-references "references")
     ("b" xref-go-back "go back to previous location"))
    "code action"
    (("n" lsp-rename "rename")
     ("c" lsp-execute-code-action "code action"))))
 :custom
 ((lsp-ui-sideline-show-diagnostics . t)
  (lsp-ui-sideline-show-code-actions . t)
  (lsp-ui-sideline-update-mode . t)
  (lsp-ui-doc-enable . t)
  (lsp-ui-doc-position . 'top)
  (lsp-ui-doc-side . 'right)
  (lsp-ui-doc-show-with-cursor . t)
  (lsp-ui-doc-show-with-mouse . nil)
  (lsp-ui-imenu-auto-refresh . t)))

; grammar check
(leaf flycheck
  :ensure t
  :global-minor-mode global-flycheck-mode)

(leaf highlight-indent-guides
  :ensure t
  :hook ((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode))

;; (with-eval-after-load 'dap-mode
;;   ;; keybind
;;   (define-key dap-mode-map (kbd "C-c d") #'dap-breakpoint-toggle)

;;   ;; hooks
;;   (add-hook 'dap-mode-hook #'dap-ui-mode)
;;   (add-hook 'dap-mode-hook #'dap-ui-controls-mode)
;;   (add-hook 'dap-mode-hook #'tooltip-mode)
;;   (add-hook 'dap-mode-hook #'dap-tooltip-mode)
;;   (add-hook 'dap-stopped-hook #'(lambda (arg) (call-interactively #'dap-hydra))))

; python
(leaf python-mode
  :ensure t)


(leaf pet
  :ensure t
  :hook
  (python-mode-hook . (lambda () (pet-mode)
                       (setq-local python-shell-interpreter (pet-executable-find "python"))
                       (setq-local python-shell-virtualenv-root (pet-virtualenv-root))
                       (setq-local lsp-pyright-venv-path python-shell-virtualenv-root)
                       (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter)
                       (setq-local lsp-ruff-server-command (list (pet-executable-find "ruff") "server"))
                       (setq-local lsp-ruff-python-path python-shell-interpreter)
                       (setq-local ruff-format-command (pet-executable-find "ruff"))
                       (pet-flycheck-setup)
                       )))

(leaf lsp-pyright
  :ensure t
  :custom ((lsp-pyright-langserver-command . "pyright")
           (lsp-pyright-disable-tagged-hints . t)
           (lsp-pyright-basedpyright-inlay-hints-variable-types . nil)))

;(leaf ein
;  :ensure t)
;;; undoを有効化 (customizeから設定しておいたほうが良さげ)
;(setq ein:worksheet-enable-undo t)
;;; 画像をインライン表示 (customizeから設定しておいたほうが良さげ)
;(setq ein:output-area-inlined-images t)
;(declare-function ein:format-time-string "ein-utils")
;(declare-function smartrep-define-key "smartrep")
; yaml

(leaf yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(leaf nix-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)))

;docker
(leaf dockerfile-mode
  :ensure t)

; Latex
(leaf yatex
  :ensure t
  :doc "new latex mode"
  :commands (yatex-mode)
  :mode (("\\.tex$" . yatex-mode)
           ("\\.ltx$" . yatex-mode)
           ("\\.cls$" . yatex-mode)
           ("\\.sty$" . yatex-mode)
           ("\\.clo$" . yatex-mode)
           ("\\.bbl$" . yatex-mode)
           ("\\.bib$" . yatex-mode))
  :custom
  (( YaTeX-inhibit-prefix-letter . t)
   (tex-command . "platex -kanji=utf8")
     ( YaTeX-dvi2-command-ext-alist .
     '(("Skim" . ".pdf")))
     ( dvi2-command . "open -a Skim")
     ( tex-pdfview-command . "open -a Skim")))

(leaf flyspell
  ;; flyspellをインストールする
  :ensure t
  ;; YaTeXモードでflyspellを使う
  :hook (yatex-mode-hook . flyspell-mode))

(leaf reftex
   :ensure t 
    :hook (yatex-mode-hook . (lambda () (reftex-mode))))

(add-hook 'reftex-mode-hook (lambda () (setq reftex-default-bibliography
                                             (directory-files-recursively (projectile-project-root) "\\.bib$"))))

(leaf ddskk
  :ensure t
  :doc "japanese IME works in emacs"
  :bind (("C-x C-j" . skk-mode))
  :custom
  ((skk-jisyo . "~/Documents/skk-jisyo.utf-8")
   (skk-large-jisyo . "~/.cache/dpp/repos/github.com/skk-dev/dict/SKK-JISYO.L")
   (skk-use-azik . t)
   (skk-search-katakana . t)
   (skk-preload . t)
   (skk-share-private-jisyo . t)
   (default-input-method . "japanese-skk")
   (skk-server-host . "localhost")
   (skk-server-portnum . 1178)))

(leaf ellama
  :ensure t
  :bind ((git-commit-mode-map
          ("M-g" . ellama-generate-commit-message)))
  :custom ((ellama-language . "Japanese")))

(leaf *hydra-goto2
  :doc "Search and move cursor"
  :bind ("M-j" . *hydra-goto2/body)
  :pretty-hydra
  ((:title "↗ Goto" :color blue :quit-key "q" :foreign-keys warn :separator "╌")
   ("Got"
    (("i" avy-goto-char       "char")
     ("t" avy-goto-char-timer "timer")
     ("l" avy-goto-line       "line")
     ("j" avy-resume          "resume"))
    "Line"
    (("h" avy-goto-line        "head")
     ("e" avy-goto-end-of-line "end")
     ("n" consult-goto-line    "number"))
    "Topic"
    (("o"  consult-outline      "outline")
     ("m"  consult-imenu        "imenu")
     ("gm" consult-global-imenu "global imenu"))
    "Error"
    ((","  lsp-bridge-diagnostic-jump-prev "previous")
     ("."  lsp-bridge-diagnostic-jump-next "next")
     ("L"  lsp-bridge-diagnostic-list "list"))
    "Spell"
    ((">"  flyspell-goto-next-error "next" :exit nil)
     ("cc" flyspell-correct-at-point "correct" :exit nil)))))

(leaf *hydra-toggle2
  :doc "Toggle functions"
  :bind ("M-t" . *hydra-toggle2/body)
  :pretty-hydra
  ((:title " Toggle" :color blue :quit-key "q" :foreign-keys warn :separator "-")
   ("Basic"
    (("v" view-mode "view mode" :toggle t)
     ("w" whitespace-mode "whitespace" :toggle t)
     ("W" whitespace-cleanup "whitespace cleanup")
     ("r" rainbow-mode "rainbow" :toggle t)
     ("b" beacon-mode "beacon" :toggle t))
    "Line & Column"
    (("l" toggle-truncate-lines "truncate line" :toggle t)
     ("n" display-line-numbers-mode "line number" :toggle t)
     ("F" display-fill-column-indicator-mode "column indicator" :toggle t)
     ("f" visual-fill-column-mode "visual column" :toggle t)
     ("c" toggle-visual-fill-column-center "fill center"))
    "Highlight"
    (("h" highlight-symbol "highligh symbol" :toggle t)
     ("L" hl-line-mode "line" :toggle t)
     ("t" hl-todo-mode "todo" :toggle t)
     ("g" git-gutter-mode "git gutter" :toggle t)
     ("i" highlight-indent-guides-mode "indent guide" :toggle t))
    "Window"
    (("t" toggle-window-transparency "transparency" :toggle t)
     ("m" toggle-window-maximize "maximize" :toggle t)
     ("p" presentation-mode "presentation" :toggle t)))))

(leaf *hydra-search
  :doc "Search functions"
  :bind
  ("C-s" . *hydra-search/body)
  :pretty-hydra
  ((:title "🔍 Search" :color blue :quit-key "q" :foreign-keys warn :separator "╌")
   ("Buffer"
    (("l" consult-line "line")
     ("o" consult-outline "outline")
     ("m" consult-imenu "imenu"))
    "Project"
    (("f" affe-find "find")
     ("r" affe-grep "grep"))
    "Document"
    (("df" consult-find-doc "find")
     ("dd" consult-grep-doc "grep")))))

(leaf *hydra-git
  :bind
  ("M-g" . *hydra-git/body)
  :pretty-hydra
  ((:title " Git" :color blue :quit-key "q" :foreign-keys warn :separator "╌")
   ("Basic"
    (("w" magit-checkout "checkout")
     ("s" magit-status "status")
     ("b" magit-branch "branch")
     ("F" magit-pull "pull")
     ("f" magit-fetch "fetch")
     ("A" magit-apply "apply")
     ("c" magit-commit "commit")
     ("P" magit-push "push"))
    ""
    (("d" magit-diff "diff")
     ("l" magit-log "log")
     ("r" magit-rebase "rebase")
     ("z" magit-stash "stash")
     ("!" magit-run "run shell command")
     ("y" magit-show-refs "references"))
    "Hunk"
    (("," git-gutter:previous-hunk "previous" :exit nil)
     ("." git-gutter:next-hunk "next" :exit nil)
     ("g" git-gutter:stage-hunk "stage")
     ("v" git-gutter:revert-hunk "revert")
     ("p" git-gutter:popup-hunk "popup"))
    " GitHub"
    (("C" checkout-gh-pr "checkout PR")
     ("o" browse-at-remote-or-copy"browse at point")
     ("k" browse-at-remote-kill "copy url")
     ("O" (shell-command "hub browse") "browse repository")))))

;; (leaf oj
 ;;  :ensure t
 ;;  :custom ((oj-compiler-python . "pypy")
 ;;           (oj-default-online-judge . 'atcoder)
 ;;           (oj-home-dir . "~/Program/Atcoder")))

(defcustom marimo:server-command "marimo edit"
  "The default command to start marimo server."
  :group 'marimo
  :type 'string)

(defcustom marimo:default-notebook-directory ""
  "The default marimo notebook directory"
  :group 'marimo
  :type 'string)

(defun marimo:server-start (server-command
                            notebook-directory
                            &optional no-login-p login-callback port)
  "Start SERVER-COMMAND with `--notebookdirectory` NOTEBOOK-DIRECTORY

Login after connection established unless NO-LOGIN0P is set.
LOGIN-CALLBACK takes two arguments, the buffer created by `marimo:notebooklist-open--finish`, and the url-or-port argument of `marimo:notebooklist-open`"
  (interactive
   (list (let ((default-command (executable-find marimo:server-command)))
           (let ((default-dir marimo:default-notebook-directory)
                 result)
             (while (or(not result) (not (file-directory-p result)))
               (setq result (read-directory-name
                             (format "%sNotebook directory: "
                                     (if result
                                         (format "[%s not a directory]" result)
                                       ""))
                             default-dir default-dir t)))
             result)
           nil
           (lambda (buffer _url-or-port)
             (pop-to-buffer buffer))
           nil))
   (when (marimo:server-process)
     (error "marimo:server-start: First `M-x marimo:stop`"))
   
   )
  )
