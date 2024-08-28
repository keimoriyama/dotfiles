(leaf py-isort :ensure t)

(leaf elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (remove-hook 'elpy-modules 'elpy-module-highlight-indentation) ;; インデントハイライトの無効化
  (remove-hook 'elpy-modules 'elpy-module-flymake) ;; flymakeの無効化
  :custom
  (elpy-rpc-python-command . "python3") ;; https://mako-note.com/ja/elpy-rpc-python-version/の問題を回避するための設定
  (flycheck-python-flake8-executable . "flake8")
  :bind (elpy-mode-map
         ("C-c C-r f" . elpy-format-code))
  :hook ((elpy-mode-hook . flycheck-mode))
)
