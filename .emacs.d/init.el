;;; init.el:
(require 'org)
(defvar my-config-dir user-emacs-directory)
(org-babel-load-file (expand-file-name "my-init.org" my-config-dir))
