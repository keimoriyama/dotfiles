;;; agent-shell-provider-usage.el --- Provider usage display for agent-shell -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'subr-x)

(defcustom my-agent-shell-codex-sessions-directory
  (expand-file-name "sessions" (or (getenv "CODEX_HOME") "~/.codex"))
  "Directory containing Codex rollout JSONL files."
  :type 'directory)

(defvar my-agent-shell--codex-rate-limit-cache nil)
(defvar-local my-agent-shell--claude-rate-limits nil)

(defvar my-agent-shell--rate-limit-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'my-agent-shell-show-provider-usage)
    map))

(defun my-agent-shell-show-provider-usage ()
  "Show account usage and time-window rate limits for the current agent.

Claude Code exposes this information through /usage, while Codex exposes it
through /status.  Queue the command when the agent is busy."
  (interactive)
  (let* ((config (agent-shell-get-config (current-buffer)))
         (identifier (map-elt config :identifier))
         (command
          (pcase identifier
            ('claude-code "/usage")
            ('codex "/status")
            (_ (user-error "Usage and rate limits are unavailable for %s"
                           (or identifier "this agent"))))))
    (agent-shell-prompt-queue command)))

(defconst my-agent-shell--claude-rate-limit-windows
  '(("five_hour" . "5h")
    ("seven_day" . "7d")
    ("seven_day_opus" . "7d-opus")
    ("seven_day_sonnet" . "7d-sonnet"))
  "Claude `unifiedWindows' keys and their labels, in mode-line order.")

(defun my-agent-shell--claude-rate-limit-label (window-name)
  "Return the mode-line label for WINDOW-NAME, or nil if unrecognized.
WINDOW-NAME is a symbol or string, the `rateLimitType' reported by a
Claude `rate_limit_event', e.g. `five_hour' or \"seven_day_opus\"."
  (cdr (assoc (if (symbolp window-name) (symbol-name window-name) window-name)
              my-agent-shell--claude-rate-limit-windows)))

(cl-defun my-agent-shell--capture-claude-rate-limit
    (&key state acp-update &allow-other-keys)
  "Capture Claude rate-limit data from ACP-UPDATE into the buffer in STATE.

Each `rate_limit_event' reports a single window's utilization and reset
time directly on `_claude/rateLimit', keyed by `rateLimitType'; it is not
nested under a `unifiedWindows' map. A window is stored one at a time as
its own event arrives."
  (when-let* ((metadata (map-elt acp-update '_meta))
              (info (or (map-elt metadata '_claude/rateLimit)
                        (map-elt metadata "_claude/rateLimit")))
              (label (my-agent-shell--claude-rate-limit-label
                      (map-elt info 'rateLimitType)))
              (utilization (map-elt info 'utilization))
              (buffer (map-elt state :buffer)))
    (with-current-buffer buffer
      (let ((used (round (* utilization (if (<= utilization 1) 100 1))))
            (reset (map-elt info 'resetsAt)))
        (setf (alist-get label my-agent-shell--claude-rate-limits
                         nil nil #'equal)
              (list :label label :used used :reset reset)))
      (force-mode-line-update))))

(defun my-agent-shell--read-codex-rate-limits ()
  "Read the newest account rate limits from Codex rollout files."
  (when (file-directory-p my-agent-shell-codex-sessions-directory)
    (let ((files
           (sort (directory-files-recursively
                  my-agent-shell-codex-sessions-directory "\\.jsonl\\'")
                 (lambda (left right)
                   (time-less-p (file-attribute-modification-time
                                 (file-attributes right))
                                (file-attribute-modification-time
                                 (file-attributes left)))))))
      (catch 'limits
        (dolist (file files)
          (with-temp-buffer
            (let* ((size (file-attribute-size (file-attributes file)))
                   (start (max 0 (- size 262144))))
              (insert-file-contents-literally file nil start)
              (goto-char (point-max))
              (while (re-search-backward "\"rate_limits\"" nil t)
                (let* ((line (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position)))
                       (record (ignore-errors
                                 (json-parse-string line :object-type 'alist)))
                       (limits (map-nested-elt record '(payload rate_limits))))
                  (when limits
                    (let (windows)
                      (dolist (key '(primary secondary))
                        (when-let* ((window (map-elt limits key))
                                    (minutes (map-elt window 'window_minutes)))
                          (push (list :label
                                      (pcase minutes
                                        (300 "5h")
                                        (10080 "7d")
                                        (_ (format "%dm" minutes)))
                                      :used (when-let ((used
                                                        (map-elt window 'used_percent)))
                                              (round used))
                                      :reset (map-elt window 'resets_at))
                                windows)))
                      (when windows
                        (throw 'limits (nreverse windows))))))))))))))

(defun my-agent-shell--format-rate-limits (limits &optional now)
  "Format LIMITS for the mode line relative to NOW."
  (when limits
    (let ((now (or now (float-time))))
      (concat
       " ["
       (string-join
        (mapcar
         (lambda (window)
           (let* ((used (plist-get window :used))
                  (reset (plist-get window :reset))
                  (reset-seconds (when (numberp reset)
                                   (if (> reset 100000000000)
                                       (/ reset 1000.0)
                                     reset)))
                  (remaining (when reset-seconds (- reset-seconds now)))
                  (remaining-text
                   (cond
                    ((not remaining) "-")
                    ((<= remaining 0) "now")
                    ((< remaining 3600) (format "%dm" (ceiling (/ remaining 60))))
                    ((< remaining 86400) (format "%dh" (ceiling (/ remaining 3600))))
                    (t (format "%dd" (ceiling (/ remaining 86400))))))
                  (face (cond ((and used (>= used 90)) 'error)
                              ((and used (>= used 70)) 'warning)
                              (t 'success))))
             (format "%s %s↻%s"
                     (plist-get window :label)
                     (propertize (if used (format "%d%%" used) "--%")
                                 'face face)
                     remaining-text)))
         limits)
        " · ")
       "]"))))

(defun my-agent-shell-rate-limit-mode-line ()
  "Return the current agent's time-window rate limits for the mode line."
  (condition-case nil
      (let* ((identifier
              (map-elt (agent-shell-get-config (current-buffer)) :identifier))
             (limits
              (pcase identifier
                ('claude-code
                 (delq nil
                       (mapcar (lambda (window)
                                 (alist-get (cdr window)
                                            my-agent-shell--claude-rate-limits
                                            nil nil #'equal))
                               my-agent-shell--claude-rate-limit-windows)))
                ('codex
                 (let ((checked-at (plist-get
                                    my-agent-shell--codex-rate-limit-cache
                                    :checked-at)))
                   (when (or (not checked-at)
                             (> (- (float-time) checked-at) 60))
                     (setq my-agent-shell--codex-rate-limit-cache
                           (list :checked-at (float-time)
                                 :limits
                                 (my-agent-shell--read-codex-rate-limits))))
                   (plist-get my-agent-shell--codex-rate-limit-cache :limits)))))
             (text (my-agent-shell--format-rate-limits limits)))
        (when text
          ;; The mode line reads % as a format spec, so double it to show one.
          (propertize (string-replace "%" "%%" text)
                      'help-echo "mouse-1: show detailed provider usage"
                      'mouse-face 'mode-line-highlight
                      'local-map my-agent-shell--rate-limit-mode-line-map)))
    (error nil)))

(defvar my-agent-shell--rate-limit-mode-line-spec
  '(:eval (my-agent-shell-rate-limit-mode-line))
  "Mode-line construct inserted next to the buffer name by
`my-agent-shell-provider-usage-setup'.")

(defun my-agent-shell-provider-usage-setup ()
  "Show the provider rate-limit segment next to the buffer name."
  (setq-local mode-line-format (copy-tree mode-line-format))
  (let ((tail (memq 'mode-line-buffer-identification mode-line-format)))
    (when (and tail
               (not (member my-agent-shell--rate-limit-mode-line-spec
                            mode-line-format)))
      (setcdr tail (cons my-agent-shell--rate-limit-mode-line-spec
                          (cdr tail))))))

(provide 'agent-shell-provider-usage)
;;; agent-shell-provider-usage.el ends here
