;;; emacs-parquet-explorer-tests.el --- Tests for emacs-parquet-explorer -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-parquet-explorer)

(ert-deftest emacs-parquet-explorer-sidecar-starts-bundled-binary ()
  (let* ((sc (emacs-parquet-explorer--sidecar-make))
         (file "/tmp/data.parquet")
         started)
    (cl-letf (((symbol-function 'file-executable-p)
               (lambda (path)
                 (should (equal path (emacs-parquet-explorer--filter-bin)))
                 t))
              ((symbol-function 'emacs-parquet-explorer--sidecar-start)
               (lambda (actual-sc actual-file)
                 (setq started (cons actual-sc actual-file)))))
      (should (eq t (emacs-parquet-explorer--sidecar-ensure sc file)))
      (should (equal started (cons sc file))))))

(ert-deftest emacs-parquet-explorer-sidecar-reports-missing-binary-to-ui ()
  (let* ((sc (emacs-parquet-explorer--sidecar-make
              :session '(:id "test")
              :pending '(:token 7)))
         result)
    (cl-letf (((symbol-function 'file-executable-p) #'ignore)
              ((symbol-function 'emacs-parquet-explorer--sidecar-start)
               (lambda (&rest _) (ert-fail "must not start a missing binary")))
              ((symbol-function 'make-process)
               (lambda (&rest _) (ert-fail "must not build inside the Nix store")))
              ((symbol-function 'emacs-parquet-explorer--send-result)
               (lambda (session token &rest kv)
                 (setq result (list session token kv)))))
      (should-not (emacs-parquet-explorer--sidecar-ensure sc "/tmp/data.parquet"))
      (should-not (emacs-parquet-explorer--sidecar-pending sc))
      (should (equal result
                     '((:id "test") 7
                       (:error "native filter binary unavailable")))))))

;;; emacs-parquet-explorer-tests.el ends here
