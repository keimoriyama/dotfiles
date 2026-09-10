;;; agent-shell-session-name.el --- Summarised buffer names for agent-shell -*- lexical-binding: t; -*-

;;; Commentary:

;; Names agent-shell buffers "Agent@Project 要約" so a buffer list says what
;; each session is doing.  The summary cannot come from the session itself:
;; agent-shell's `session-title-changed' carries only the raw first prompt for
;; Claude Code, which supplies no title over ACP.  So the first prompt is fed
;; to a separate one-shot CLI, the same trick agent-shell-dashboard uses for
;; its "Needs you" sub-line.

(require 'cl-lib)
(require 'map)
(require 'subr-x)

(defcustom my-agent-shell-session-name-summary-command
  '("claude" "-p" "--model" "haiku" "--allowed-tools" "")
  "Command (program + args) run to summarise a session's first prompt.
The prompt is written to stdin and the summary read from stdout."
  :type '(repeat string))

(defcustom my-agent-shell-session-name-summary-length 24
  "Maximum number of characters kept from a summary."
  :type 'integer)

(defcustom my-agent-shell-session-name-prompt-limit 1500
  "Maximum number of characters of the first prompt sent to the summariser."
  :type 'integer)

(defvar-local my-agent-shell-session-name--base nil
  "Buffer name without a summary, captured before the first rename.")

(defun my-agent-shell-session-name-format (agent-name project-name)
  "Format an agent-shell buffer name from AGENT-NAME and PROJECT-NAME."
  (format "%s@%s" agent-name project-name))

(defun my-agent-shell-session-name--shorten (text)
  "Return TEXT as a single line of at most the configured length.
Returns nil when TEXT holds nothing usable."
  (when (stringp text)
    (let* ((line (string-trim (car (split-string text "\n" t))))
           (limit my-agent-shell-session-name-summary-length))
      (unless (string-empty-p line)
        (if (> (length line) limit)
            (concat (substring line 0 limit) "…")
          line)))))

(defun my-agent-shell-session-name--compose (base summary)
  "Join BASE and SUMMARY into a buffer name, or return BASE alone."
  (if summary (format "%s %s" base summary) base))

(defun my-agent-shell-session-name--summary-prompt (prompt)
  "Build the summariser input asking for a short label for PROMPT."
  (concat "次の依頼を日本語の名詞句1行で要約してください。"
          "20文字以内、句読点や記号や前置きは付けないでください。\n\n---\n"
          (if (> (length prompt) my-agent-shell-session-name-prompt-limit)
              (substring prompt 0 my-agent-shell-session-name-prompt-limit)
            prompt)))

(defun my-agent-shell-session-name--rename (buffer summary)
  "Rename BUFFER to its base name plus SUMMARY."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (shell-maker-set-buffer-name
       buffer
       (my-agent-shell-session-name--compose
        my-agent-shell-session-name--base summary)))))

(defun my-agent-shell-session-name--summarise (buffer prompt)
  "Rename BUFFER once a summary of PROMPT comes back from the summariser."
  (when (executable-find (car my-agent-shell-session-name-summary-command))
    (let* ((output (generate-new-buffer " *agent-shell-session-name*"))
           (errors (generate-new-buffer " *agent-shell-session-name-stderr*"))
           ;; Run outside the project so the summariser's own session log
           ;; does not land in ~/.claude/projects next to the real work.
           (default-directory temporary-file-directory)
           (process
            (make-process
             :name "agent-shell-session-name"
             :buffer output
             :noquery t
             :connection-type 'pipe
             :command my-agent-shell-session-name-summary-command
             :stderr errors
             :sentinel
             (lambda (process _event)
               (unless (process-live-p process)
                 (when (buffer-live-p output)
                   (when-let* (((zerop (process-exit-status process)))
                               (summary (my-agent-shell-session-name--shorten
                                         (with-current-buffer output
                                           (buffer-string)))))
                     (my-agent-shell-session-name--rename buffer summary))
                   (kill-buffer output))
                 (when (buffer-live-p errors)
                   (kill-buffer errors)))))))
      (process-send-string process
                           (my-agent-shell-session-name--summary-prompt prompt))
      (process-send-eof process))))

(defun my-agent-shell-session-name--on-first-prompt (buffer token event)
  "Rename BUFFER from the prompt in EVENT and drop subscription TOKEN."
  (agent-shell-unsubscribe :subscription token)
  (let ((prompt (map-nested-elt event '(:data :prompt))))
    (when (and (stringp prompt) (not (string-empty-p prompt)))
      ;; Show the prompt's head straight away; the summary replaces it later.
      (my-agent-shell-session-name--rename
       buffer (my-agent-shell-session-name--shorten prompt))
      (my-agent-shell-session-name--summarise buffer prompt))))

(defun my-agent-shell-session-name-setup ()
  "Rename this agent-shell buffer once its first prompt is submitted."
  (unless my-agent-shell-session-name--base
    (setq my-agent-shell-session-name--base (buffer-name))
    (let* ((buffer (current-buffer))
           (token nil))
      (setq token
            (agent-shell-subscribe-to
             :shell-buffer buffer
             :event 'input-submitted
             :on-event
             (lambda (event)
               (my-agent-shell-session-name--on-first-prompt buffer token event)))))))

(provide 'agent-shell-session-name)
;;; agent-shell-session-name.el ends here
