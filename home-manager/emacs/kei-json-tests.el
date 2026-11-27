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

(define-derived-mode kei-json-tests-mode prog-mode "JSON-test"
  "Stand-in for `json-ts-mode', wired up the way init.org wires it."
  (add-hook 'after-revert-hook #'kei/json-format-on-open nil t))
(add-hook 'kei-json-tests-mode-hook #'kei/json-format-on-open)

(defmacro kei-json-tests--with-file (var contents &rest body)
  "Visit a fresh JSON file holding CONTENTS with VAR bound to its name."
  (declare (indent 2))
  `(let* ((,var (make-temp-file "kei-json" nil ".json" ,contents))
          (auto-mode-alist (cons '("\\.json\\'" . kei-json-tests-mode)
                                 auto-mode-alist))
          (buffer nil))
     (unwind-protect
         (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
           (setq buffer (find-file-noselect ,var))
           (with-current-buffer buffer ,@body))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (delete-file ,var))))

(ert-deftest kei-json-revert-keeps-the-file-text-as-the-raw-source ()
  "A plain revert formats once, so toggling still shows the file's text."
  (kei-json-tests--with-file file "{\"a\":1}"
    (should (equal "{\n  \"a\": 1\n}\n" (buffer-string)))
    (revert-buffer t t)
    (should (equal "{\n  \"a\": 1\n}\n" (buffer-string)))
    (kei/json-toggle-raw)
    (should (equal "{\"a\":1}" (buffer-string)))))

(ert-deftest kei-json-revert-preserving-modes-formats-the-new-contents ()
  "Auto-revert keeps the mode, so the hook alone must reformat the new text."
  (kei-json-tests--with-file file "{\"a\":1}"
    (with-temp-file file (insert "{\"b\":2}"))
    (revert-buffer t t t)
    (should (equal "{\n  \"b\": 2\n}\n" (buffer-string)))
    (kei/json-toggle-raw)
    (should (equal "{\"b\":2}" (buffer-string)))))

(ert-deftest kei-json-revert-to-the-formatted-text-shows-it-as-raw ()
  "A file rewritten to exactly the formatted view reverts to that text as raw."
  (kei-json-tests--with-file file "{\"a\":1}"
    (with-temp-file file (insert "{\n  \"a\": 1\n}\n"))
    (revert-buffer t t t)
    (kei/json-toggle-raw)
    (should (equal "{\n  \"a\": 1\n}\n" (buffer-string)))))

(ert-deftest kei-json-toggle-keeps-text-outside-a-narrowing ()
  "Toggling while narrowed acts on the whole buffer instead of erasing
what lies outside the narrowed part."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (&rest _) nil)))
    (with-temp-buffer
      (insert "{\"a\":{\"b\":1},\"c\":2}")
      (narrow-to-region 6 13)
      (kei/json-toggle-raw)
      (should (equal "{\n  \"a\": {\n    \"b\": 1\n  },\n  \"c\": 2\n}\n"
                     (buffer-string)))
      (kei/json-toggle-raw)
      (should (equal "{\"a\":{\"b\":1},\"c\":2}" (buffer-string))))))

(provide 'kei-json-tests)
;;; kei-json-tests.el ends here
