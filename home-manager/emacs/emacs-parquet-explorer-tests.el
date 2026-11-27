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

(ert-deftest emacs-egui-decodes-utf8-query-values ()
  (let* ((params
          (url-parse-query-string
           "payload=%7B%22value%22%3A%22%E3%81%82%22%7D"))
         (payload-json (car (assoc-default "payload" params)))
         (payload (json-read-from-string
                   (emacs-egui--decode-utf8-query-value payload-json)))
         (value (cdr (assq 'value payload))))
    (should (equal value "あ"))
    (should (multibyte-string-p value))))

(ert-deftest emacs-egui-preserves-percent-escapes-in-query-values ()
  (let* ((params
          (url-parse-query-string
           "payload=%7B%22value%22%3A%22100%2520%22%7D"))
         (payload-json (car (assoc-default "payload" params)))
         (payload (json-read-from-string
                   (emacs-egui--decode-utf8-query-value payload-json))))
    (should (equal (cdr (assq 'value payload)) "100%20"))))

;;; emacs-parquet-explorer-tests.el ends here
