#!/usr/bin/env bash
set -euo pipefail

test_root=$(git rev-parse --show-toplevel)
export NIPPO_ORG_JOURNAL_TEST_ROOT="$test_root"

emacsclient --eval '
(progn
  (load
   (expand-file-name
    "home-manager/emacs/nippo-org-journal-tests.el"
    (getenv "NIPPO_ORG_JOURNAL_TEST_ROOT"))
   nil t)
  (let ((stats (ert-run-tests-batch "^nippo-org-journal-")))
    (when (> (ert-stats-completed-unexpected stats) 0)
      (error "nippo Org Journal tests failed"))
    t))'
