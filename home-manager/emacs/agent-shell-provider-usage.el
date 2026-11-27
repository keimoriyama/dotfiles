;;; agent-shell-provider-usage.el --- Provider usage display for agent-shell -*- lexical-binding: t; -*-

;;; Commentary:

;; Shows Codex's time-window rate limits next to the buffer name, and sends
;; the current agent's own usage command.  Claude's windows are not here:
;; Claude Code hands those percentages only to its status line, so
;; `claude-usage' polls that tool and shows them in Claude buffers instead.

(require 'agent-usage-format)
(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function agent-shell-get-config "agent-shell")
(declare-function agent-shell-prompt-queue "agent-shell-prompt-queue")

(defgroup my-agent-shell-provider-usage nil
  "Provider usage display for agent-shell."
  :group 'agent-shell)

(defcustom my-agent-shell-codex-sessions-directory
  (expand-file-name "sessions" (or (getenv "CODEX_HOME") "~/.codex"))
  "Directory containing Codex rollout JSONL files."
  :type 'directory)

(defvar my-agent-shell--codex-rollouts-scanned 10
  "Number of newest Codex rollout files searched for rate limits.")

(defconst my-agent-shell--codex-rollout-tail-bytes 262144
  "Number of trailing bytes read from each Codex rollout file.")

(defconst my-agent-shell--codex-rate-limit-ttl 60
  "Seconds a read of the Codex rate limits is reused.")

(defvar my-agent-shell--codex-rate-limit-cache nil
  "Plist of :checked-at and :limits from the last Codex rollout read.")

(defvar my-agent-shell--rate-limit-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'my-agent-shell-show-provider-usage)
    map))

(defun my-agent-shell-show-provider-usage (&optional event)
  "Show account usage and time-window rate limits for the current agent.

Claude Code exposes this information through /usage, while Codex exposes it
through /status.  Queue the command when the agent is busy.

When EVENT is a mouse click, act on the buffer of the window clicked:
a click on another window's mode line does not select that window."
  (interactive (list last-nonmenu-event))
  (let ((window (and (mouse-event-p event) (posn-window (event-start event)))))
    (with-current-buffer (if (windowp window) (window-buffer window) (current-buffer))
      (let* ((identifier (map-elt (agent-shell-get-config (current-buffer))
                                  :identifier))
             (command
              (pcase identifier
                ('claude-code "/usage")
                ('codex "/status")
                (_ (user-error "Usage and rate limits are unavailable for %s"
                               (or identifier "this agent"))))))
        (agent-shell-prompt-queue command)))))

;; LIMITATION: only the newest rollouts are searched, so a run of sessions
;; that never reported limits hides older figures.  Searching every file
;; stalls redisplay for accounts that never report any.
(defun my-agent-shell--newest-codex-rollouts ()
  "Return the newest Codex rollout files, newest first."
  (when (file-directory-p my-agent-shell-codex-sessions-directory)
    (let ((dated (mapcar (lambda (file)
                           (cons (file-attribute-modification-time
                                  (file-attributes file))
                                 file))
                         (directory-files-recursively
                          my-agent-shell-codex-sessions-directory
                          "\\.jsonl\\'"))))
      (mapcar #'cdr
              (seq-take (sort dated (lambda (left right)
                                      (time-less-p (car right) (car left))))
                        my-agent-shell--codex-rollouts-scanned)))))

(defun my-agent-shell--codex-windows (limits)
  "Return the time windows in a Codex LIMITS object, primary first."
  (seq-keep
   (lambda (key)
     (when-let* ((window (map-elt limits key))
                 (minutes (map-elt window 'window_minutes)))
       (list :label (pcase minutes
                      (300 "5h")
                      (10080 "7d")
                      (_ (format "%dm" minutes)))
             :used (when-let* ((used (map-elt window 'used_percent)))
                     (round used))
             :reset (map-elt window 'resets_at))))
   '(primary secondary)))

(defun my-agent-shell--read-codex-rate-limits ()
  "Read the newest account rate limits from Codex rollout files."
  (catch 'limits
    (dolist (file (my-agent-shell--newest-codex-rollouts))
      (with-temp-buffer
        (let ((size (file-attribute-size (file-attributes file))))
          (insert-file-contents-literally
           file nil (max 0 (- size my-agent-shell--codex-rollout-tail-bytes))))
        (goto-char (point-max))
        (while (re-search-backward "\"rate_limits\"" nil t)
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 ;; Read JSON null as nil so a missing snapshot or window
                 ;; is skipped rather than signalling in `map-elt'.
                 (record (ignore-errors
                           (json-parse-string line :object-type 'alist
                                              :null-object nil)))
                 (windows (my-agent-shell--codex-windows
                           (map-nested-elt record '(payload rate_limits)))))
            (when windows
              (throw 'limits windows))))))))

(defun my-agent-shell--cached-codex-rate-limits ()
  "Return the Codex rate limits, rereading the rollouts at most once a TTL.
A failed read counts as no limits until the TTL passes, so a broken
rollout is not rescanned on every redisplay."
  (let ((checked-at (plist-get my-agent-shell--codex-rate-limit-cache
                               :checked-at)))
    (when (or (not checked-at)
              (> (- (float-time) checked-at)
                 my-agent-shell--codex-rate-limit-ttl))
      (setq my-agent-shell--codex-rate-limit-cache
            (list :checked-at (float-time)
                  :limits (ignore-errors
                            (my-agent-shell--read-codex-rate-limits))))))
  (plist-get my-agent-shell--codex-rate-limit-cache :limits))

(defun my-agent-shell-rate-limit-mode-line ()
  "Return the current agent's time-window rate limits for the mode line.
Only Codex reports these over a channel this can read."
  (condition-case nil
      (when-let* (((eq (map-elt (agent-shell-get-config (current-buffer))
                                :identifier)
                       'codex))
                  (text (agent-usage-format-windows
                         (my-agent-shell--cached-codex-rate-limits))))
        ;; The mode line reads % as a format spec, so double it to show one.
        (propertize (string-replace "%" "%%" text)
                    'help-echo "mouse-1: show detailed provider usage"
                    'mouse-face 'mode-line-highlight
                    'local-map my-agent-shell--rate-limit-mode-line-map))
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
