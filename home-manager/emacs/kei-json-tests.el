;;; kei-json-tests.el --- Tests for kei-json -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "kei-json.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest kei-json-toggle-formats-with-jq-and-restores-raw ()
  "jq formatting keeps the exact raw text and modification state."
  (skip-unless (executable-find "jq"))
  (let ((raw "{ \"name\": \"Emacs\", \"items\": [1,2] }"))
    (with-temp-buffer
      (insert raw)
      (set-buffer-modified-p nil)
      (kei/json-toggle-raw)
      (should (equal "{\n  \"name\": \"Emacs\",\n  \"items\": [\n    1,\n    2\n  ]\n}\n"
                     (buffer-string)))
      (should kei/json--formatted-view)
      (should-not (buffer-modified-p))
      (kei/json-toggle-raw)
      (should (equal raw (buffer-string)))
      (should-not kei/json--formatted-view)
      (should-not (buffer-modified-p)))))

(ert-deftest kei-json-toggle-preserves-edits-made-in-the-formatted-view ()
  "Switching to raw does not replace newer edits with the opened source."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (&rest _) nil)))
    (with-temp-buffer
      (insert "{\"a\":1}")
      (set-buffer-modified-p nil)
      (kei/json-toggle-raw)
      (goto-char (point-min))
      (search-forward ": 1")
      (replace-match ": 2")
      (let ((edited (buffer-string)))
        (kei/json-toggle-raw)
        (should (equal edited (buffer-string)))
        (should-not kei/json--formatted-view)
        (should (buffer-modified-p))
        (kei/json-toggle-raw)
        (should kei/json--formatted-view)
        (should (equal edited (buffer-string)))))))

(ert-deftest kei-json-toggle-formats-with-the-built-in-fallback ()
  "A missing jq executable does not prevent formatted/raw switching."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (&rest _) nil)))
    (with-temp-buffer
      (insert "{\"a\":1,\"b\":[2]}")
      (kei/json-toggle-raw)
      (should (equal "{\n  \"a\": 1,\n  \"b\": [\n    2\n  ]\n}\n"
                     (buffer-string)))
      (kei/json-toggle-raw)
      (should (equal "{\"a\":1,\"b\":[2]}" (buffer-string))))))

(ert-deftest kei-json-toggle-keeps-invalid-json-unchanged ()
  "An invalid JSON buffer is not partially replaced when formatting fails."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (&rest _) nil)))
    (with-temp-buffer
      (insert "{\"a\":")
      (should-error (kei/json-toggle-raw))
      (should (equal "{\"a\":" (buffer-string)))
      (should-not kei/json--raw-source)
      (should-not kei/json--formatted-view))))

(ert-deftest kei-json-open-formats-json-but-preserves-special-inputs ()
  "Opening formats JSON, but skips JSON Lines and retains malformed JSON."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (&rest _) nil)))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/kei-json-test.json")
      (insert "{\"a\":1}")
      (kei/json-format-on-open)
      (should (equal "{\n  \"a\": 1\n}\n" (buffer-string)))
      (should kei/json--formatted-view))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/kei-json-test.jsonl")
      (insert "{\"a\":1}\n{\"a\":2}\n")
      (kei/json-format-on-open)
      (should (equal "{\"a\":1}\n{\"a\":2}\n" (buffer-string)))
      (should-not kei/json--formatted-view))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/kei-json-invalid.json")
      (insert "{\"a\":")
      (kei/json-format-on-open)
      (should (equal "{\"a\":" (buffer-string)))
      (should-not kei/json--formatted-view))))

(provide 'kei-json-tests)
;;; kei-json-tests.el ends here
