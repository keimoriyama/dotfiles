(leaf py-isort :ensure t)
(leaf flymake-ruff
  :ensure t
  :hook (eglot-managed-mode-hook .(lambda ()
                                    (when (derived-mode-p 'python-mode 'python-ts-mode)
                                      (flymake-ruff-load)))))

(leaf highlight-indent-guides
  :ensure t
  :hook ((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode)
  )
(leaf python
  :custom (python-indent-guess-indent-offset-verbose . nil)
  :hook (python-ts-mode-hook . eglot-ensure))
(leaf lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))
