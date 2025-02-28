;;; init.el:
(require 'org)
(defvar my-config-dir user-emacs-directory)
(org-babel-load-file (expand-file-name "my-init.org" my-config-dir))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/Documents/org-mode/projects/LLMのデータ拡張.org" "/Users/kei/Documents/org-mode/projects/ACL2024.org" "/Users/kei/Documents/org-mode/projects/修論発表と博士入試.org" "/Users/kei/Documents/org-mode/projects/研究計画書_SPRINGGX.org"))
 '(org-preview-latex-process-alist
   '((dvipng :programs
             ("latex" "dvipng")
             :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
             (2.0 . 2.0)
             :latex-compiler
             ("latex -interaction nonstopmode -output-directory %o %f")
             :image-converter
             ("dvipng -D %D -T tight -o %O %f")
             :transparent-image-converter
             ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
     (dvisvgm :programs
              ("latex" "dvisvgm")
              :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
              (1.7 . 1.5)
              :latex-compiler
              ("latex -interaction nonstopmode -output-directory %o %f")
              :image-converter
              ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
     (imagemagick :programs
                  ("latex" "convert")
                  :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                  (1.0 . 1.0)
                  :latex-compiler
                  ("pdflatex -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("convert -density %D -trim -antialias %f -quality 100 %O"))))
 '(package-selected-packages
   '(ddskk reftex flyspell yatex dockerfile-mode yaml-mode lsp-pyright pet ruff-format python-mode highlight-indent-guides flycheck lsp-ui lsp-mode emacs-lsp-booster ox-gfm yasnippet tempel orderless affe embark-consult consult avy-zap avy marginalia vertico cape nerd-icons-corfu corfu exec-path-from-shell smerge-mode magit which-key spaceline iflipb puni free-keys rainbow-delimiters git-gutter multiple-cursors expand-region bufferlo centaur-tabs projectile volatile-highlights solarized-theme nerd-icons-completion f dash blackout el-get pretty-hydra hydra leaf-keywords leaf))
 '(package-vc-selected-packages
   '((emacs-lsp-booster :url "https://github.com/blahgeek/emacs-lsp-booster"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
