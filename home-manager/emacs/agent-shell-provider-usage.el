;;; agent-shell-provider-usage.el --- Provider usage display for agent-shell -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'subr-x)

(defcustom my-agent-shell-codex-sessions-directory
  (expand-file-name "sessions" (or (getenv "CODEX_HOME") "~/.codex"))
  "Directory containing Codex rollout JSONL files."
  :type 'directory)

(defcustom my-agent-shell-claude-rate-limit-file
  (expand-file-name "claude-code/rate-limits.json"
                    (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "File where Claude Code's status line leaves its `rate_limits' object.
Written by home-manager/claude-code/statusline.sh; the path has to match."
  :type 'file)

(defcustom my-agent-shell-claude-rate-limit-staleness 900
  "Seconds after which the values in `my-agent-shell-claude-rate-limit-file' are dimmed.
Only a Claude Code terminal session refreshes that file, so a percentage can
outlive the usage it described."
  :type 'integer)

(defvar my-agent-shell--codex-rate-limit-cache nil)
(defvar my-agent-shell--claude-rate-limit-file-cache nil)
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
    ("seven_day_sonnet" . "7d-sonnet")
    ("overage" . "overage"))
  "Claude `rateLimitType' values and their labels, in mode-line order.")

(defconst my-agent-shell--claude-rate-limit-statuses
  '(("allowed" . "ok")
    ("allowed_warning" . "warn")
    ("rejected" . "blocked"))
  "Claude rate-limit `status' values and their mode-line words.")

(defconst my-agent-shell--claude-raw-sdk-message-filter
  [((type . "rate_limit_event"))]
  "`emitRawSDKMessages' filter asking Claude Code for rate-limit events only.
The option also accepts t for every message, but one type is all this
needs and keeps the rest of the SDK stream off the wire.")

(defun my-agent-shell--claude-rate-limit-label (window-name)
  "Return the mode-line label for WINDOW-NAME, or nil if unrecognized.
WINDOW-NAME is a symbol or string, the `rateLimitType' reported by a
Claude `rate_limit_event', e.g. `five_hour' or \"seven_day_opus\"."
  (cdr (assoc (if (symbolp window-name) (symbol-name window-name) window-name)
              my-agent-shell--claude-rate-limit-windows)))

(defun my-agent-shell--store-claude-rate-limit (info buffer)
  "Store the rate-limit window described by INFO in BUFFER.

INFO is one `rate_limit_info' object: a single window keyed by
`rateLimitType', with its `resetsAt' alongside and its `utilization' only
once Claude Code is warning about that window. Windows accumulate across
events; the status stands in for the percentage until one arrives."
  (when-let* ((label (my-agent-shell--claude-rate-limit-label
                      (map-elt info 'rateLimitType)))
              (buffer (and (buffer-live-p buffer) buffer)))
    (with-current-buffer buffer
      (let* ((utilization (map-elt info 'utilization))
             (used (when (numberp utilization)
                     (round (* utilization (if (<= utilization 1) 100 1)))))
             (reset (map-elt info 'resetsAt)))
        (setf (alist-get label my-agent-shell--claude-rate-limits
                         nil nil #'equal)
              (list :label label :used used :reset reset
                    :status (map-elt info 'status))))
      (force-mode-line-update))))

;; LIMITATION: nothing Claude Code sends over ACP carries a percentage during
;; ordinary use. It caches the numbers from the API's rate-limit response
;; headers and hands them only to its own terminal status line, so the
;; everyday percentages come from the file that status line writes, and these
;; events supply the window Claude Code is actively warning about.
;; https://github.com/anthropics/claude-code/issues/50518 (closed, not planned)
(cl-defun my-agent-shell--capture-claude-rate-limit
    (&key state acp-update &allow-other-keys)
  "Capture Claude rate-limit data from ACP-UPDATE into the buffer in STATE.

The window lives directly on `_claude/rateLimit' in the update's `_meta',
not nested under a `unifiedWindows' map."
  (when-let* ((metadata (map-elt acp-update '_meta))
              (info (or (map-elt metadata '_claude/rateLimit)
                        (map-elt metadata "_claude/rateLimit"))))
    (my-agent-shell--store-claude-rate-limit info (map-elt state :buffer))))

(cl-defun my-agent-shell--capture-claude-raw-rate-limit
    (&key state acp-notification &allow-other-keys)
  "Capture a raw Claude rate-limit event from ACP-NOTIFICATION into STATE.

Sessions that asked for `emitRawSDKMessages' receive the SDK's own
`rate_limit_event' as a `_claude/sdkMessage' notification, which carries
the utilization the session updates omit."
  (when (equal (map-elt acp-notification 'method) "_claude/sdkMessage")
    (when-let* ((message (map-nested-elt acp-notification '(params message)))
                (info (map-elt message 'rate_limit_info)))
      (my-agent-shell--store-claude-rate-limit info (map-elt state :buffer)))))

(defun my-agent-shell-request-claude-rate-limit-events (config)
  "Return CONFIG with raw rate-limit events requested for new Claude sessions.

Claude Code reads `emitRawSDKMessages' out of the session's `_meta', so the
request has to be in place before `session/new'; existing sessions keep
whatever they were created with."
  (let* ((config (copy-tree config))
         (meta (map-elt config :session-meta))
         (claude (map-elt meta 'claudeCode)))
    (setf (alist-get 'emitRawSDKMessages claude)
          my-agent-shell--claude-raw-sdk-message-filter)
    (setf (alist-get 'claudeCode meta) claude)
    (setf (alist-get :session-meta config) meta)
    config))

(defun my-agent-shell--read-claude-rate-limit-file ()
  "Read the windows Claude Code's status line last wrote, newest first.

Every window in the file is returned, so a window this build has a label for
shows up without the reader knowing about it in advance.  `:stale' marks
values old enough that only a long-gone terminal session could have written
them."
  (when-let* ((attributes (file-attributes my-agent-shell-claude-rate-limit-file))
              (limits (with-temp-buffer
                        (ignore-errors
                          (insert-file-contents
                           my-agent-shell-claude-rate-limit-file)
                          (json-parse-string (buffer-string)
                                             :object-type 'alist)))))
    (let ((stale (> (float-time
                     (time-subtract
                      nil (file-attribute-modification-time attributes)))
                    my-agent-shell-claude-rate-limit-staleness)))
      (delq nil
            (mapcar
             (lambda (window)
               (when-let* ((label (my-agent-shell--claude-rate-limit-label
                                   (car window)))
                           (used (map-elt (cdr window) 'used_percentage)))
                 (list :label label
                       :used (round used)
                       :reset (map-elt (cdr window) 'resets_at)
                       :stale stale)))
             limits)))))

(defun my-agent-shell--claude-mode-line-windows ()
  "Return the current buffer's Claude rate-limit windows, in mode-line order.

The status-line file is the only source that carries a percentage during
ordinary use, so it supplies the baseline; a window captured live over ACP
replaces it, being both fresher and the one that knows about warnings."
  (let ((checked-at (plist-get my-agent-shell--claude-rate-limit-file-cache
                               :checked-at)))
    (when (or (not checked-at) (> (- (float-time) checked-at) 60))
      (setq my-agent-shell--claude-rate-limit-file-cache
            (list :checked-at (float-time)
                  :limits (my-agent-shell--read-claude-rate-limit-file)))))
  (let ((from-file (plist-get my-agent-shell--claude-rate-limit-file-cache
                              :limits)))
    (delq nil
          (mapcar (lambda (window)
                    (let ((label (cdr window)))
                      (or (alist-get label my-agent-shell--claude-rate-limits
                                     nil nil #'equal)
                          (seq-find (lambda (w) (equal (plist-get w :label) label))
                                    from-file))))
                  my-agent-shell--claude-rate-limit-windows))))

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
                  (status (plist-get window :status))
                  (face (cond ((plist-get window :stale) 'shadow)
                              ((and used (>= used 90)) 'error)
                              ((and used (>= used 70)) 'warning)
                              ((equal status "rejected") 'error)
                              ((equal status "allowed_warning") 'warning)
                              (t 'success)))
                  ;; Claude Code withholds the percentage until it is warning
                  ;; about a window, so fall back to the status it does send.
                  (value (cond (used (format "%d%%" used))
                               (status (or (cdr (assoc
                                                 status
                                                 my-agent-shell--claude-rate-limit-statuses))
                                           status))
                               (t "--%"))))
             (format "%s %s↻%s"
                     (plist-get window :label)
                     (propertize value 'face face)
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
                ('claude-code (my-agent-shell--claude-mode-line-windows))
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
