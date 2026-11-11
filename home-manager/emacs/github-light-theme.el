;;; github-light-theme.el --- Light theme modeled on GitHub's Primer colors -*- lexical-binding: t; -*-

;;; Commentary:

;; A light theme that follows the colors GitHub uses on github.com.
;; The UI colors come from the Primer "light" functional tokens and the
;; syntax colors from Primer's prettylights syntax tokens.  The layout
;; (a named palette plus one face list) follows catppuccin-theme.

;;; Code:

(deftheme github-light
  "Light theme modeled on GitHub's Primer colors.")

(defconst github-light-colors
  '(;; Backgrounds and borders.
    (bg            . "#ffffff")   ; bgColor-default
    (bg-muted      . "#f6f8fa")   ; bgColor-muted
    (bg-emphasis   . "#eff2f5")   ; bgColor-neutral-muted over white
    (bg-hl         . "#f6f8fa")
    (bg-region     . "#cce5ff")
    (border        . "#d1d9e0")   ; borderColor-default
    (border-muted  . "#e1e6eb")
    ;; Foregrounds.
    (fg            . "#1f2328")   ; fgColor-default
    (fg-muted      . "#59636e")   ; fgColor-muted
    (fg-subtle     . "#818b98")
    (fg-disabled   . "#afb8c1")
    ;; Semantic colors.
    (accent        . "#0969da")   ; fgColor-accent
    (success       . "#1a7f37")   ; fgColor-success
    (attention     . "#9a6700")   ; fgColor-attention
    (severe        . "#bc4c00")   ; fgColor-severe
    (danger        . "#d1242f")   ; fgColor-danger
    (done          . "#8250df")   ; fgColor-done
    (sponsors      . "#bf3989")   ; fgColor-sponsors
    ;; Muted backgrounds for the semantic colors.
    (accent-bg     . "#ddf4ff")
    (success-bg    . "#dafbe1")
    (attention-bg  . "#fff8c5")
    (severe-bg     . "#fff1e5")
    (danger-bg     . "#ffebe9")
    (done-bg       . "#fbefff")
    ;; Syntax (prettylights).
    (keyword       . "#cf222e")
    (string        . "#0a3069")
    (constant      . "#0550ae")
    (entity        . "#6639ba")
    (function      . "#8250df")
    (variable      . "#953800")
    (comment       . "#59636e")
    (regexp        . "#116329")
    (tag           . "#116329")
    ;; Diff (prettylights markup).
    (diff-add-fg   . "#116329")
    (diff-add-bg   . "#dafbe1")
    (diff-add-hl   . "#aceebb")
    (diff-del-fg   . "#82071e")
    (diff-del-bg   . "#ffebe9")
    (diff-del-hl   . "#ffcecb")
    (diff-chg-fg   . "#953800")
    (diff-chg-bg   . "#ffd8b5"))
  "Named colors of the GitHub Light theme.")

(defun github-light-color (name)
  "Return the hex string of the GitHub Light color NAME."
  (or (alist-get name github-light-colors)
      (error "Unknown github-light color: %s" name)))

(let ((class '((class color) (min-colors 89)))
      (bg           (github-light-color 'bg))
      (bg-muted     (github-light-color 'bg-muted))
      (bg-emphasis  (github-light-color 'bg-emphasis))
      (bg-hl        (github-light-color 'bg-hl))
      (bg-region    (github-light-color 'bg-region))
      (border       (github-light-color 'border))
      (border-muted (github-light-color 'border-muted))
      (fg           (github-light-color 'fg))
      (fg-muted     (github-light-color 'fg-muted))
      (fg-subtle    (github-light-color 'fg-subtle))
      (fg-disabled  (github-light-color 'fg-disabled))
      (accent       (github-light-color 'accent))
      (success      (github-light-color 'success))
      (attention    (github-light-color 'attention))
      (severe       (github-light-color 'severe))
      (danger       (github-light-color 'danger))
      (done         (github-light-color 'done))
      (sponsors     (github-light-color 'sponsors))
      (accent-bg    (github-light-color 'accent-bg))
      (success-bg   (github-light-color 'success-bg))
      (attention-bg (github-light-color 'attention-bg))
      (severe-bg    (github-light-color 'severe-bg))
      (danger-bg    (github-light-color 'danger-bg))
      (done-bg      (github-light-color 'done-bg))
      (keyword      (github-light-color 'keyword))
      (string       (github-light-color 'string))
      (constant     (github-light-color 'constant))
      (entity       (github-light-color 'entity))
      (function     (github-light-color 'function))
      (variable     (github-light-color 'variable))
      (comment      (github-light-color 'comment))
      (regexp       (github-light-color 'regexp))
      (tag          (github-light-color 'tag))
      (diff-add-fg  (github-light-color 'diff-add-fg))
      (diff-add-bg  (github-light-color 'diff-add-bg))
      (diff-add-hl  (github-light-color 'diff-add-hl))
      (diff-del-fg  (github-light-color 'diff-del-fg))
      (diff-del-bg  (github-light-color 'diff-del-bg))
      (diff-del-hl  (github-light-color 'diff-del-hl))
      (diff-chg-fg  (github-light-color 'diff-chg-fg))
      (diff-chg-bg  (github-light-color 'diff-chg-bg)))
  (custom-theme-set-faces
   'github-light

   ;; Basic UI.
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,accent))))
   `(fringe ((,class (:background ,bg :foreground ,fg-disabled))))
   `(vertical-border ((,class (:foreground ,border))))
   `(window-divider ((,class (:foreground ,border))))
   `(window-divider-first-pixel ((,class (:foreground ,border))))
   `(window-divider-last-pixel ((,class (:foreground ,border))))
   `(internal-border ((,class (:background ,bg-muted))))
   `(child-frame-border ((,class (:background ,border))))
   `(border ((,class (:background ,border))))
   `(shadow ((,class (:foreground ,fg-subtle))))
   `(link ((,class (:foreground ,accent :underline t))))
   `(link-visited ((,class (:foreground ,done :underline t))))
   `(highlight ((,class (:background ,accent-bg))))
   `(hl-line ((,class (:background ,bg-hl :extend t))))
   `(region ((,class (:background ,bg-region :extend t))))
   `(secondary-selection ((,class (:background ,attention-bg :extend t))))
   `(minibuffer-prompt ((,class (:foreground ,accent :weight bold))))
   `(header-line ((,class (:background ,bg-muted :foreground ,fg
                           :box (:line-width 4 :color ,bg-muted)))))
   `(tooltip ((,class (:background ,bg-muted :foreground ,fg))))
   `(trailing-whitespace ((,class (:background ,danger-bg))))
   `(escape-glyph ((,class (:foreground ,sponsors))))
   `(homoglyph ((,class (:foreground ,sponsors))))
   `(nobreak-space ((,class (:foreground ,fg-disabled :underline t))))
   `(fill-column-indicator ((,class (:foreground ,border-muted))))
   `(separator-line ((,class (:background ,border :height 0.1))))
   `(success ((,class (:foreground ,success :weight bold))))
   `(warning ((,class (:foreground ,attention :weight bold))))
   `(error ((,class (:foreground ,danger :weight bold))))
   `(match ((,class (:background ,attention-bg :foreground ,fg))))
   `(help-key-binding ((,class (:background ,bg-muted :foreground ,accent
                                :box (:line-width -1 :color ,border)))))

   ;; Mode line and tab bar.
   `(mode-line ((,class (:background ,bg-muted :foreground ,fg
                         :box (:line-width 1 :color ,border)))))
   `(mode-line-active ((,class (:inherit mode-line))))
   `(mode-line-inactive ((,class (:background ,bg :foreground ,fg-subtle
                                  :box (:line-width 1 :color ,border-muted)))))
   `(mode-line-buffer-id ((,class (:weight bold))))
   `(mode-line-emphasis ((,class (:foreground ,accent :weight bold))))
   `(mode-line-highlight ((,class (:background ,accent-bg))))
   `(tab-bar ((,class (:background ,bg-muted :foreground ,fg-muted))))
   `(tab-bar-tab ((,class (:background ,bg :foreground ,fg :weight bold
                           :box (:line-width 1 :color ,border)))))
   `(tab-bar-tab-inactive ((,class (:background ,bg-muted :foreground ,fg-muted))))
   `(tab-line ((,class (:background ,bg-muted :foreground ,fg-muted))))

   ;; Line numbers.
   `(line-number ((,class (:background ,bg :foreground ,fg-disabled))))
   `(line-number-current-line ((,class (:background ,bg-hl :foreground ,fg))))
   `(line-number-major-tick ((,class (:foreground ,fg-subtle))))
   `(line-number-minor-tick ((,class (:foreground ,fg-disabled))))

   ;; Search.
   `(isearch ((,class (:background ,attention :foreground ,bg :weight bold))))
   `(isearch-fail ((,class (:background ,danger-bg :foreground ,danger))))
   `(lazy-highlight ((,class (:background ,attention-bg :foreground ,fg))))
   `(query-replace ((,class (:inherit isearch))))

   ;; Parentheses.
   `(show-paren-match ((,class (:background ,accent-bg :foreground ,accent :weight bold))))
   `(show-paren-mismatch ((,class (:background ,danger-bg :foreground ,danger :weight bold))))

   ;; Syntax.
   `(font-lock-builtin-face ((,class (:foreground ,constant))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((,class (:inherit font-lock-comment-face))))
   `(font-lock-doc-face ((,class (:foreground ,string))))
   `(font-lock-doc-markup-face ((,class (:foreground ,constant))))
   `(font-lock-constant-face ((,class (:foreground ,constant))))
   `(font-lock-number-face ((,class (:foreground ,constant))))
   `(font-lock-escape-face ((,class (:foreground ,regexp))))
   `(font-lock-function-name-face ((,class (:foreground ,function))))
   `(font-lock-function-call-face ((,class (:foreground ,function))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword))))
   `(font-lock-negation-char-face ((,class (:foreground ,keyword))))
   `(font-lock-operator-face ((,class (:foreground ,keyword))))
   `(font-lock-preprocessor-face ((,class (:foreground ,keyword))))
   `(font-lock-property-name-face ((,class (:foreground ,constant))))
   `(font-lock-property-use-face ((,class (:foreground ,constant))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,regexp :weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,regexp :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,string))))
   `(font-lock-type-face ((,class (:foreground ,entity))))
   `(font-lock-variable-name-face ((,class (:foreground ,variable))))
   `(font-lock-variable-use-face ((,class (:foreground ,fg))))
   `(font-lock-bracket-face ((,class (:foreground ,fg))))
   `(font-lock-delimiter-face ((,class (:foreground ,fg))))
   `(font-lock-punctuation-face ((,class (:foreground ,fg))))
   `(font-lock-misc-punctuation-face ((,class (:foreground ,fg-muted))))
   `(font-lock-warning-face ((,class (:foreground ,severe :weight bold))))

   ;; Completion: built-in, vertico, orderless, marginalia, consult, corfu.
   `(completions-common-part ((,class (:foreground ,accent :weight bold))))
   `(completions-first-difference ((,class (:foreground ,keyword))))
   `(completions-annotations ((,class (:foreground ,fg-muted))))
   `(vertico-current ((,class (:background ,accent-bg :extend t))))
   `(vertico-group-title ((,class (:foreground ,fg-muted :weight bold))))
   `(vertico-group-separator ((,class (:foreground ,border :strike-through t))))
   `(orderless-match-face-0 ((,class (:foreground ,accent :weight bold))))
   `(orderless-match-face-1 ((,class (:foreground ,done :weight bold))))
   `(orderless-match-face-2 ((,class (:foreground ,success :weight bold))))
   `(orderless-match-face-3 ((,class (:foreground ,severe :weight bold))))
   `(marginalia-documentation ((,class (:foreground ,fg-muted))))
   `(marginalia-key ((,class (:foreground ,accent))))
   `(marginalia-file-priv-dir ((,class (:foreground ,accent))))
   `(consult-file ((,class (:foreground ,fg))))
   `(consult-preview-line ((,class (:background ,bg-hl :extend t))))
   `(consult-highlight-match ((,class (:inherit lazy-highlight))))
   `(corfu-default ((,class (:background ,bg-muted :foreground ,fg))))
   `(corfu-current ((,class (:background ,accent-bg :foreground ,fg))))
   `(corfu-border ((,class (:background ,border))))
   `(corfu-bar ((,class (:background ,fg-disabled))))
   `(corfu-annotations ((,class (:foreground ,fg-muted))))
   `(embark-keybinding ((,class (:foreground ,accent :weight bold))))

   ;; which-key, hydra, avy.
   `(which-key-key-face ((,class (:foreground ,accent :weight bold))))
   `(which-key-separator-face ((,class (:foreground ,fg-subtle))))
   `(which-key-command-description-face ((,class (:foreground ,fg))))
   `(which-key-group-description-face ((,class (:foreground ,done))))
   `(which-key-local-map-description-face ((,class (:foreground ,variable))))
   `(hydra-face-red ((,class (:foreground ,keyword :weight bold))))
   `(hydra-face-blue ((,class (:foreground ,accent :weight bold))))
   `(hydra-face-amaranth ((,class (:foreground ,sponsors :weight bold))))
   `(hydra-face-pink ((,class (:foreground ,sponsors :weight bold))))
   `(hydra-face-teal ((,class (:foreground ,success :weight bold))))
   `(pretty-hydra-toggle-on-face ((,class (:foreground ,success))))
   `(pretty-hydra-toggle-off-face ((,class (:foreground ,fg-subtle))))
   `(avy-lead-face ((,class (:background ,accent :foreground ,bg :weight bold))))
   `(avy-lead-face-0 ((,class (:background ,done :foreground ,bg :weight bold))))
   `(avy-lead-face-1 ((,class (:background ,fg-muted :foreground ,bg :weight bold))))
   `(avy-lead-face-2 ((,class (:background ,sponsors :foreground ,bg :weight bold))))
   `(avy-background-face ((,class (:foreground ,fg-disabled))))

   ;; Highlighting helpers.
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,fg-muted))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,accent))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,done))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,success))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,severe))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,sponsors))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,constant))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,attention))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,entity))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,danger :weight bold))))
   `(vhl/default-face ((,class (:background ,attention-bg))))
   `(symbol-overlay-default-face ((,class (:background ,accent-bg))))
   `(beacon-fallback-background ((,class (:background ,accent))))

   ;; Diagnostics and spelling.
   `(flymake-error ((,class (:underline (:style wave :color ,danger)))))
   `(flymake-warning ((,class (:underline (:style wave :color ,attention)))))
   `(flymake-note ((,class (:underline (:style wave :color ,accent)))))
   `(flyspell-incorrect ((,class (:underline (:style wave :color ,danger)))))
   `(flyspell-duplicate ((,class (:underline (:style wave :color ,attention)))))
   `(eglot-highlight-symbol-face ((,class (:background ,accent-bg))))
   `(eglot-inlay-hint-face ((,class (:foreground ,fg-subtle :height 0.9))))

   ;; Diff, magit, git-gutter.
   `(diff-added ((,class (:background ,diff-add-bg :foreground ,diff-add-fg :extend t))))
   `(diff-removed ((,class (:background ,diff-del-bg :foreground ,diff-del-fg :extend t))))
   `(diff-changed ((,class (:background ,diff-chg-bg :foreground ,diff-chg-fg :extend t))))
   `(diff-refine-added ((,class (:background ,diff-add-hl :foreground ,diff-add-fg))))
   `(diff-refine-removed ((,class (:background ,diff-del-hl :foreground ,diff-del-fg))))
   `(diff-header ((,class (:background ,bg-muted :foreground ,fg))))
   `(diff-file-header ((,class (:background ,bg-muted :foreground ,fg :weight bold))))
   `(diff-hunk-header ((,class (:background ,done-bg :foreground ,done))))
   `(diff-indicator-added ((,class (:foreground ,success))))
   `(diff-indicator-removed ((,class (:foreground ,danger))))
   `(magit-section-heading ((,class (:foreground ,fg :weight bold))))
   `(magit-section-highlight ((,class (:background ,bg-hl :extend t))))
   `(magit-branch-local ((,class (:foreground ,accent))))
   `(magit-branch-remote ((,class (:foreground ,success))))
   `(magit-branch-current ((,class (:foreground ,accent :weight bold
                                    :box (:line-width -1 :color ,accent)))))
   `(magit-tag ((,class (:foreground ,attention))))
   `(magit-hash ((,class (:foreground ,fg-muted))))
   `(magit-dimmed ((,class (:foreground ,fg-subtle))))
   `(magit-log-author ((,class (:foreground ,variable))))
   `(magit-log-date ((,class (:foreground ,fg-muted))))
   `(magit-diff-file-heading ((,class (:foreground ,fg :weight bold))))
   `(magit-diff-file-heading-highlight ((,class (:background ,bg-muted :weight bold))))
   `(magit-diff-hunk-heading ((,class (:background ,bg-muted :foreground ,fg-muted :extend t))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,done-bg :foreground ,done :extend t))))
   `(magit-diff-context ((,class (:foreground ,fg-muted :extend t))))
   `(magit-diff-context-highlight ((,class (:background ,bg-muted :foreground ,fg :extend t))))
   `(magit-diff-added ((,class (:background ,diff-add-bg :foreground ,diff-add-fg :extend t))))
   `(magit-diff-added-highlight ((,class (:background ,diff-add-bg :foreground ,diff-add-fg :extend t))))
   `(magit-diff-removed ((,class (:background ,diff-del-bg :foreground ,diff-del-fg :extend t))))
   `(magit-diff-removed-highlight ((,class (:background ,diff-del-bg :foreground ,diff-del-fg :extend t))))
   `(magit-diffstat-added ((,class (:foreground ,success))))
   `(magit-diffstat-removed ((,class (:foreground ,danger))))
   `(git-gutter:added ((,class (:foreground ,success :weight bold))))
   `(git-gutter:deleted ((,class (:foreground ,danger :weight bold))))
   `(git-gutter:modified ((,class (:foreground ,severe :weight bold))))

   ;; Org.
   `(org-document-title ((,class (:foreground ,fg :weight bold :height 1.3))))
   `(org-document-info ((,class (:foreground ,fg-muted))))
   `(org-document-info-keyword ((,class (:foreground ,fg-subtle))))
   `(org-level-1 ((,class (:foreground ,fg :weight bold))))
   `(org-level-2 ((,class (:foreground ,constant :weight bold))))
   `(org-level-3 ((,class (:foreground ,function :weight bold))))
   `(org-level-4 ((,class (:foreground ,variable :weight bold))))
   `(org-level-5 ((,class (:foreground ,success :weight bold))))
   `(org-level-6 ((,class (:foreground ,sponsors :weight bold))))
   `(org-level-7 ((,class (:foreground ,attention :weight bold))))
   `(org-level-8 ((,class (:foreground ,fg-muted :weight bold))))
   `(org-todo ((,class (:foreground ,success :weight bold))))
   `(org-done ((,class (:foreground ,done :weight bold))))
   `(org-headline-done ((,class (:foreground ,fg-subtle))))
   `(org-tag ((,class (:foreground ,fg-muted :weight normal))))
   `(org-date ((,class (:foreground ,accent :underline t))))
   `(org-link ((,class (:inherit link))))
   `(org-block ((,class (:background ,bg-muted :extend t))))
   `(org-block-begin-line ((,class (:background ,bg-muted :foreground ,fg-subtle :extend t))))
   `(org-block-end-line ((,class (:inherit org-block-begin-line))))
   `(org-code ((,class (:background ,bg-emphasis :foreground ,fg))))
   `(org-verbatim ((,class (:background ,bg-emphasis :foreground ,keyword))))
   `(org-quote ((,class (:foreground ,fg-muted :slant italic))))
   `(org-table ((,class (:foreground ,fg))))
   `(org-formula ((,class (:foreground ,constant))))
   `(org-checkbox ((,class (:foreground ,accent))))
   `(org-meta-line ((,class (:foreground ,fg-subtle))))
   `(org-special-keyword ((,class (:foreground ,fg-subtle))))
   `(org-drawer ((,class (:foreground ,fg-subtle))))
   `(org-scheduled ((,class (:foreground ,fg))))
   `(org-scheduled-today ((,class (:foreground ,accent :weight bold))))
   `(org-scheduled-previously ((,class (:foreground ,severe))))
   `(org-upcoming-deadline ((,class (:foreground ,attention))))
   `(org-warning ((,class (:foreground ,danger :weight bold))))
   `(org-agenda-structure ((,class (:foreground ,fg :weight bold))))
   `(org-agenda-date ((,class (:foreground ,constant))))
   `(org-agenda-date-today ((,class (:foreground ,accent :weight bold))))
   `(org-agenda-date-weekend ((,class (:foreground ,fg-muted))))
   `(org-agenda-done ((,class (:foreground ,fg-subtle))))
   `(org-agenda-clocking ((,class (:background ,attention-bg))))
   `(org-superstar-header-bullet ((,class (:foreground ,fg-muted))))
   `(org-superstar-item ((,class (:foreground ,fg-muted))))

   ;; Markdown.
   `(markdown-header-face ((,class (:foreground ,constant :weight bold))))
   `(markdown-code-face ((,class (:background ,bg-muted :extend t))))
   `(markdown-inline-code-face ((,class (:background ,bg-emphasis))))
   `(markdown-link-face ((,class (:foreground ,accent))))
   `(markdown-url-face ((,class (:foreground ,string :underline t))))
   `(markdown-list-face ((,class (:foreground ,severe))))

   ;; Dired.
   `(dired-directory ((,class (:foreground ,accent :weight bold))))
   `(dired-symlink ((,class (:foreground ,done))))
   `(dired-marked ((,class (:foreground ,success :weight bold))))
   `(dired-flagged ((,class (:foreground ,danger :weight bold))))
   `(dired-ignored ((,class (:foreground ,fg-subtle))))

   ;; Dashboard.
   `(dashboard-banner-logo-title ((,class (:foreground ,fg :weight bold))))
   `(dashboard-heading ((,class (:foreground ,accent :weight bold))))
   `(dashboard-items-face ((,class (:foreground ,fg :weight normal))))

   ;; SKK.
   `(skk-emacs-hiragana-face ((,class (:foreground ,success))))
   `(skk-emacs-katakana-face ((,class (:foreground ,accent))))
   `(skk-emacs-jisx0208-latin-face ((,class (:foreground ,attention))))
   `(skk-emacs-abbrev-face ((,class (:foreground ,done))))
   `(skk-henkan-face-default ((,class (:background ,accent-bg :foreground ,fg))))
   `(skk-dcomp-face ((,class (:foreground ,fg-subtle))))

   ;; Terminal colors.
   `(ansi-color-black ((,class (:background ,fg :foreground ,fg))))
   `(ansi-color-red ((,class (:background ,keyword :foreground ,keyword))))
   `(ansi-color-green ((,class (:background ,diff-add-fg :foreground ,diff-add-fg))))
   `(ansi-color-yellow ((,class (:background ,attention :foreground ,attention))))
   `(ansi-color-blue ((,class (:background ,accent :foreground ,accent))))
   `(ansi-color-magenta ((,class (:background ,done :foreground ,done))))
   `(ansi-color-cyan ((,class (:background "#1b7c83" :foreground "#1b7c83"))))
   `(ansi-color-white ((,class (:background ,fg-muted :foreground ,fg-muted))))
   `(ansi-color-bright-black ((,class (:background ,fg-muted :foreground ,fg-muted))))
   `(ansi-color-bright-red ((,class (:background ,danger :foreground ,danger))))
   `(ansi-color-bright-green ((,class (:background ,success :foreground ,success))))
   `(ansi-color-bright-yellow ((,class (:background ,severe :foreground ,severe))))
   `(ansi-color-bright-blue ((,class (:background ,constant :foreground ,constant))))
   `(ansi-color-bright-magenta ((,class (:background ,function :foreground ,function))))
   `(ansi-color-bright-cyan ((,class (:background "#3192aa" :foreground "#3192aa"))))
   `(ansi-color-bright-white ((,class (:background ,fg-subtle :foreground ,fg-subtle))))))

;; `require' でこのファイルを読んだあと `load-theme' が見つけられるようにする。
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'github-light)
(provide 'github-light-theme)

;;; github-light-theme.el ends here
