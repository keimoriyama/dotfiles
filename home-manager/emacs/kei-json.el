;;; kei-json.el --- Formatted JSON views -*- lexical-binding: t; -*-

;;; Commentary:

;; Show JSON with two-space indentation and keep the source available for
;; switching back to its raw representation.

;;; Code:

(require 'json)
(require 'subr-x)

(defvar-local kei/json--raw-source nil
  "Raw JSON text saved before displaying the formatted view.")

(defvar-local kei/json--formatted-view nil
  "Non-nil while the current buffer displays formatted JSON.")

(defvar-local kei/json--formatted-source nil
  "Formatted text from which edits can be detected before restoring raw JSON.")

(defun kei/json--format (source)
  "Return SOURCE as two-space-indented JSON.
Use jq when available, otherwise use Emacs's JSON formatter."
  (if-let ((jq (executable-find "jq")))
      (with-temp-buffer
        (insert source)
        (let ((status (call-process-region (point-min) (point-max)
                                           jq t t nil "--indent" "2" ".")))
          (unless (and (integerp status) (zerop status))
            (error "jq failed with status %s" status))
          (buffer-string)))
    (json-parse-string source)
    (with-temp-buffer
      (insert source)
      (let ((json-encoding-default-indentation "  "))
        (json-pretty-print-buffer))
      (goto-char (point-max))
      (skip-chars-backward " \t\n\r")
      (delete-region (point) (point-max))
      (insert "\n")
      (buffer-string))))

;;;###autoload
(defun kei/json-toggle-raw ()
  "Toggle the current buffer between raw and formatted JSON."
  (interactive)
  (let ((modified (buffer-modified-p))
        (position (point))
        (inhibit-read-only t))
    (if kei/json--formatted-view
        (let ((current (buffer-substring-no-properties (point-min) (point-max))))
          (erase-buffer)
          (insert (if (equal current kei/json--formatted-source)
                      kei/json--raw-source
                    current))
          (setq kei/json--formatted-view nil
                kei/json--formatted-source nil))
      (let* ((raw (buffer-substring-no-properties (point-min) (point-max)))
             (formatted (kei/json--format raw)))
        (setq kei/json--raw-source raw)
        (erase-buffer)
        (insert formatted)
        (setq kei/json--formatted-source formatted
              kei/json--formatted-view t)))
    (goto-char (min position (point-max)))
    (set-buffer-modified-p modified)
    (when (called-interactively-p 'interactive)
      (message "%s JSON" (if kei/json--formatted-view "Formatted" "Raw")))))

;;;###autoload
(defun kei/json-format-on-open ()
  "Format the current JSON file, leaving JSON Lines and invalid JSON raw."
  (when (and buffer-file-name
             (not (string-suffix-p ".jsonl" buffer-file-name)))
    (setq kei/json--raw-source nil
          kei/json--formatted-view nil
          kei/json--formatted-source nil)
    (condition-case err
        (kei/json-toggle-raw)
      (error
       (message "JSON formatting failed; keeping raw view: %s"
                (error-message-string err))))))

(provide 'kei-json)
;;; kei-json.el ends here
