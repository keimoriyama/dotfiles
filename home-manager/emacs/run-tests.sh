#!/usr/bin/env bash
set -euo pipefail

test_root=$(git rev-parse --show-toplevel)
export NIPPO_ORG_JOURNAL_TEST_ROOT="$test_root"

emacsclient --eval '
(progn
  (dolist (test-file
           (list "home-manager/emacs/nippo-org-journal-tests.el"
                 "home-manager/emacs/org-archive-tests.el"))
    (load
     (expand-file-name test-file
                       (getenv "NIPPO_ORG_JOURNAL_TEST_ROOT"))
     nil t))
  (let ((stats (ert-run-tests-batch "^\\(nippo-org-journal-\\|org-archive-\\)")))
    (when (> (ert-stats-completed-unexpected stats) 0)
      (error "Emacs Lisp tests failed"))
    t))'
