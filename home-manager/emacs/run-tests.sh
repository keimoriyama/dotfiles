#!/usr/bin/env bash
set -euo pipefail

test_root=$(git rev-parse --show-toplevel)
export DOTFILES_TEST_ROOT="$test_root"

# *-tests.el をすべて読み込み、ファイル名から -tests.el を除いた接頭辞を
# 持つテストだけを走らせる。稼働中の Emacs に定義済みの他のテストは除く。
emacsclient --eval '
(let* ((files (directory-files
               (expand-file-name "home-manager/emacs/"
                                 (getenv "DOTFILES_TEST_ROOT"))
               t "-tests\\.el\\'"'"'"))
       (prefixes (mapcar (lambda (file)
                           (concat (string-remove-suffix
                                    "-tests.el" (file-name-nondirectory file))
                                   "-"))
                         files)))
  (dolist (file files)
    (load file nil t))
  (let ((stats (ert-run-tests-batch (concat "^" (regexp-opt prefixes)))))
    (when (> (ert-stats-completed-unexpected stats) 0)
      (error "Emacs Lisp tests failed"))
    (ert-stats-total stats)))'
