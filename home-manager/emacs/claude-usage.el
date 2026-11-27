;;; claude-usage.el --- Claude subscription usage in the mode line -*- lexical-binding: t; -*-

;;; Commentary:

;; Display Claude subscription usage in the mode line by polling the
;; `claude-usage-line' CLI asynchronously.  A contract billed against a
;; spending limit reports that limit instead of the time windows, and an
;; Enterprise seat is recognised from Claude Code's own account file so a
;; failed fetch reads as an unknown spend rather than an empty window:
;;
;;     Claude [Org 61% $132/$215↻12d19h]
;;     Claude [5h 32%↻4h · 7d 47%↻2d]
;;
;; Either shape is shown in `agent-usage-format''s shared layout, so Claude
;; and Codex read the same way.  Enable with (claude-usage-mode 1).

;;; Code:

(require 'agent-usage-format)
(require 'json)
(require 'map)
(require 'subr-x)

(declare-function agent-shell-get-config "agent-shell")

(defgroup claude-usage nil
  "Claude subscription usage in the mode line."
  :group 'mode-line)

(defcustom claude-usage-command
  '("claude-usage-line" "--json")
  "Command used to retrieve Claude usage.
Claude Code hands the per-window percentages only to its status line, so
they are read back from the tool installed as that status line."
  :type '(repeat string))

(defcustom claude-usage-account-file
  "~/.claude.json"
  "Claude Code's account file, read to recognise an Enterprise seat."
  :type 'file)

(defcustom claude-usage-refresh-interval
  60
  "Refresh interval in seconds."
  :type 'integer)

(defcustom claude-usage-timeout
  30
  "Seconds a fetch may run before it is killed.
A fetch still running blocks every later one, so a CLI stuck on the
network would otherwise freeze the figures for the rest of the session."
  :type 'integer)

(defvar claude-usage--state
  (list :five-hour nil
        :seven-day nil
        :five-hour-reset nil
        :seven-day-reset nil
        :org nil
        :org-used-usd nil
        :org-limit-usd nil
        :org-currency nil
        :org-reset nil
        :enterprise nil
        :updated-at nil
        :error nil)
  "Latest known usage.
:five-hour, :seven-day and :org hold the spent percentages, the :org-*
keys the spending limit behind :org, :enterprise whether the seat is
billed against a spending limit at all, and :updated-at the time of the
last successful fetch.")

(defvar claude-usage--timer nil)
(defvar claude-usage--process nil)

(defvar claude-usage-mode-line-string ""
  "Mode-line construct showing Claude usage.")

(defvar claude-usage--mode-line-spec
  '(:eval (claude-usage--mode-line))
  "Mode-line construct that shows Claude usage in Claude buffers.")

;;;###autoload
(defun claude-usage-refresh ()
  "Fetch Claude usage now and update the mode line."
  (interactive)
  (claude-usage--fetch))

(defun claude-usage-status ()
  "Show the current Claude usage state in the echo area."
  (interactive)
  (let ((five-hour (plist-get claude-usage--state :five-hour))
        (seven-day (plist-get claude-usage--state :seven-day))
        (org (plist-get claude-usage--state :org))
        (spend (claude-usage--spend-detail claude-usage--state))
        (updated-at (plist-get claude-usage--state :updated-at))
        (error-message (plist-get claude-usage--state :error)))
    (message "Claude usage\n\n5-hour used: %s\n7-day used: %s\nOrg used: %s\nLast updated: %s\nStatus: %s%s"
             (if five-hour (format "%d%%" five-hour) "?")
             (if seven-day (format "%d%%" seven-day) "?")
             (if org (format "%d%%%s" org (or spend "")) "?")
             (if updated-at (format-time-string "%Y-%m-%d %H:%M" updated-at) "never")
             (if error-message "ERROR" "OK")
             (if error-message (concat "\nError: " error-message) ""))))

(defun claude-usage--program ()
  "Return the program name of `claude-usage-command', for messages."
  (car claude-usage-command))

(defun claude-usage--fetch ()
  "Start an asynchronous usage fetch unless one is already running."
  (unless (process-live-p claude-usage--process)
    (let ((buffer (generate-new-buffer " *claude-usage*"))
          (stderr nil))
      (condition-case err
          (let (process)
            ;; Diagnostics the CLI prints on stderr would otherwise land in
            ;; front of the JSON and make a successful run unparsable.
            (setq stderr (make-pipe-process
                          :name "claude-usage-stderr"
                          :buffer (generate-new-buffer " *claude-usage-stderr*")
                          :noquery t
                          :sentinel #'ignore))
            (setq process (make-process
                           :name "claude-usage"
                           :buffer buffer
                           :command claude-usage-command
                           :stderr stderr
                           :noquery t
                           :sentinel #'claude-usage--process-sentinel))
            (process-put process 'claude-usage-stderr stderr)
            (process-put process 'claude-usage-deadline
                         (run-at-time claude-usage-timeout nil
                                      #'claude-usage--time-out process))
            (setq claude-usage--process process)
            ;; The CLI is normally fed stdin JSON by Claude Code's status-line
            ;; caller; without an EOF here it blocks waiting for stdin and
            ;; then exits with "stdin timeout".
            (process-send-eof process))
        (error
         ;; Typically the CLI is not on PATH.
         (kill-buffer buffer)
         (claude-usage--discard-stderr stderr)
         (claude-usage--record-error (error-message-string err)))))))

(defun claude-usage--time-out (process)
  "Kill PROCESS if it is still running past `claude-usage-timeout'."
  (when (process-live-p process)
    (process-put process 'claude-usage-timed-out t)
    (delete-process process)))

(defun claude-usage--discard-stderr (stderr)
  "Delete the STDERR pipe process and its buffer."
  (when (processp stderr)
    (let ((buffer (process-buffer stderr)))
      (delete-process stderr)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun claude-usage--buffer-text (buffer)
  "Return the text of BUFFER, or the empty string when it is gone."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer (buffer-string))
    ""))

(defun claude-usage--process-sentinel (process _event)
  "Consume the output of PROCESS once it has exited.
A process that is no longer `claude-usage--process' was abandoned when
the mode was turned off, so only its resources are released."
  (when (memq (process-status process) '(exit signal))
    (let* ((stderr-process (process-get process 'claude-usage-stderr))
           (output (claude-usage--buffer-text (process-buffer process)))
           ;; LIMITATION: stderr arrives through a separate pipe and may
           ;; not be read in full by the time the main process exits.
           (stderr (string-trim
                    (claude-usage--buffer-text
                     (and stderr-process (process-buffer stderr-process))))))
      (when-let* ((deadline (process-get process 'claude-usage-deadline)))
        (cancel-timer deadline))
      (when (buffer-live-p (process-buffer process))
        (kill-buffer (process-buffer process)))
      (claude-usage--discard-stderr stderr-process)
      (when (eq process claude-usage--process)
        (setq claude-usage--process nil)
        (cond
         ((process-get process 'claude-usage-timed-out)
          (claude-usage--record-error
           (format "%s timed out after %ds"
                   (claude-usage--program) claude-usage-timeout)))
         ((not (eq (process-status process) 'exit))
          (claude-usage--record-error
           (format "%s was killed by a signal" (claude-usage--program))))
         ((not (zerop (process-exit-status process)))
          (claude-usage--record-error
           (format "%s exited with status %d%s"
                   (claude-usage--program)
                   (process-exit-status process)
                   (if (string-empty-p stderr) "" (concat ": " stderr)))))
         (t (claude-usage--handle-output output)))))))

(defun claude-usage--handle-output (output)
  "Install the usage reported in OUTPUT, or record the failure."
  (if (string-blank-p output)
      (claude-usage--record-error
       (format "%s produced no output" (claude-usage--program)))
    (condition-case err
        (progn
          (setq claude-usage--state
                (plist-put (claude-usage--parse output)
                           :enterprise (claude-usage--enterprise-p)))
          (claude-usage--update-mode-line))
      (error (claude-usage--record-error (error-message-string err))))))

(defun claude-usage--record-error (message)
  "Record MESSAGE in the state and refresh the mode line."
  (setq claude-usage--state (plist-put claude-usage--state :error message))
  (claude-usage--update-mode-line))

(defvar claude-usage--enterprise-cache nil
  "Cons of the account file's modification time and the verdict read from it.")

(defun claude-usage--enterprise-p ()
  "Return non-nil when this seat is billed against a spending limit.
The account file carries every cached feature flag, so the verdict is
kept until the file itself changes."
  (let* ((file (expand-file-name claude-usage-account-file))
         (modified (and (file-readable-p file)
                        (file-attribute-modification-time
                         (file-attributes file)))))
    (cond
     ((null modified) nil)
     ((equal modified (car claude-usage--enterprise-cache))
      (cdr claude-usage--enterprise-cache))
     (t
      (let ((verdict (claude-usage--read-enterprise file)))
        (setq claude-usage--enterprise-cache (cons modified verdict))
        verdict)))))

(defun claude-usage--read-enterprise (file)
  "Return non-nil when FILE describes an Enterprise seat.
Either field alone is enough: the two agree today, so one being renamed
must not take the verdict with it."
  (condition-case nil
      (let* ((account (alist-get
                       'oauthAccount
                       (json-parse-string
                        (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))
                        :object-type 'alist :array-type 'list)))
             (organization-type (alist-get 'organizationType account))
             (seat-tier (alist-get 'seatTier account)))
        (or (equal organization-type "claude_enterprise")
            (and (stringp seat-tier)
                 (string-prefix-p "enterprise" seat-tier))))
    ;; No Claude Code on this machine, or a file written half-way.
    (error nil)))

(defun claude-usage--parse (output)
  "Parse JSON OUTPUT from `claude-usage-command' into a fresh state plist.
Signal an error on malformed input."
  (let* ((data (json-parse-string output :object-type 'alist :array-type 'list))
         (five-hour (alist-get 'five_hour data))
         (seven-day (alist-get 'seven_day data))
         ;; The CLI reports org as JSON null on a contract without one.
         (org (let ((value (alist-get 'org data)))
                (when (consp value) value)))
         (five-used (alist-get 'utilization_pct five-hour))
         (seven-used (alist-get 'utilization_pct seven-day))
         (org-used (alist-get 'utilization_pct org))
         (org-limit (claude-usage--number (alist-get 'limit_usd org)))
         (org-currency (alist-get 'currency org)))
    (unless five-hour
      (error "Output is missing five_hour"))
    (unless seven-day
      (error "Output is missing seven_day"))
    (unless (numberp five-used)
      (error "five_hour.utilization_pct is not a number"))
    (unless (numberp seven-used)
      (error "seven_day.utilization_pct is not a number"))
    (list :five-hour (round five-used)
          :seven-day (round seven-used)
          :five-hour-reset (alist-get 'resets_at five-hour)
          :seven-day-reset (alist-get 'resets_at seven-day)
          ;; A contract with no spending limit still reports a zeroed org
          ;; block, so the limit is what marks the share as a real figure.
          :org (when (and org-limit (numberp org-used)) (round org-used))
          :org-used-usd (claude-usage--number (alist-get 'used_usd org))
          :org-limit-usd org-limit
          :org-currency (when (stringp org-currency) org-currency)
          :org-reset (alist-get 'resets_at org)
          :updated-at (current-time)
          :error nil)))

(defun claude-usage--number (value)
  "Return VALUE when it is a number, nil otherwise."
  (when (numberp value) value))

(defun claude-usage--spend-detail (state)
  "Return the spend behind STATE's org percentage, or nil when unknown."
  (let ((used (plist-get state :org-used-usd))
        (limit (plist-get state :org-limit-usd))
        (currency (plist-get state :org-currency)))
    (when (and used limit)
      (if (or (null currency) (equal currency "USD"))
          (format " $%.0f/$%.0f" used limit)
        (format " %.0f/%.0f %s" used limit currency)))))

(defun claude-usage--update-mode-line ()
  "Recompute `claude-usage-mode-line-string' from the current state."
  (setq claude-usage-mode-line-string
        (claude-usage--format-mode-line claude-usage--state))
  (force-mode-line-update t))

(defun claude-usage--format-mode-line (state &optional now)
  "Return the mode-line text for STATE relative to NOW.
This string reaches the mode line by symbol indirection, where
%-constructs are not decoded, so its percent signs stay single."
  (let* ((five-hour (plist-get state :five-hour))
         (seven-day (plist-get state :seven-day))
         (org (plist-get state :org))
         (windows
          (cond
           ;; A contract billed against a spending limit leaves the time
           ;; windows at zero, so the limit is the only figure worth room.
           (org (list (list :label "Org" :used org
                            :detail (claude-usage--spend-detail state)
                            :reset (plist-get state :org-reset))))
           ;; The spending limit reaches the CLI over OAuth, which can
           ;; fail; the time windows it falls back to are zero on such a
           ;; seat and would read as an untouched quota.
           ((plist-get state :enterprise)
            (list (list :label "Org" :used nil :reset :null)))
           ((and five-hour seven-day)
            (list (list :label "5h" :used five-hour
                        :reset (plist-get state :five-hour-reset))
                  (list :label "7d" :used seven-day
                        :reset (plist-get state :seven-day-reset)))))))
    (if windows
        (concat " Claude"
                (agent-usage-format-windows windows now)
                (if (plist-get state :error) "*" ""))
      " Claude ?")))

(defun claude-usage--mode-line ()
  "Return the Claude usage construct for a Claude agent shell."
  (condition-case nil
      (when (and (derived-mode-p 'agent-shell-mode)
                 (eq (map-elt (agent-shell-get-config (current-buffer))
                              :identifier)
                     'claude-code))
        'claude-usage-mode-line-string)
    (error nil)))

(defun claude-usage--start-timer ()
  (claude-usage--stop-timer)
  (setq claude-usage--timer
        (run-at-time 0 claude-usage-refresh-interval #'claude-usage-refresh)))

(defun claude-usage--stop-timer ()
  (when claude-usage--timer
    (cancel-timer claude-usage--timer)
    (setq claude-usage--timer nil)))

(defun claude-usage--abandon-fetch ()
  "Kill the running fetch without letting it touch the state."
  (when-let* ((process claude-usage--process))
    (setq claude-usage--process nil)
    (when (process-live-p process)
      (delete-process process))))

(defun claude-usage--install-mode-line ()
  "Put the usage segment right after the buffer name.
`global-mode-string' renders at the far right, behind the minor-mode
list, which a narrow window cuts off before reaching it."
  (let ((format (copy-tree (default-value 'mode-line-format))))
    (unless (member claude-usage--mode-line-spec format)
      (when-let* ((tail (memq 'mode-line-buffer-identification format)))
        (setcdr tail (cons claude-usage--mode-line-spec (cdr tail)))
        (setq-default mode-line-format format)))))

(defun claude-usage--remove-mode-line ()
  "Take the usage segment back out of the mode line."
  (setq-default mode-line-format
                (delete claude-usage--mode-line-spec
                        (copy-tree (default-value 'mode-line-format)))))

;;;###autoload
(define-minor-mode claude-usage-mode
  "Show Claude subscription usage in the mode line."
  :global t
  :group 'claude-usage
  (if claude-usage-mode
      (progn
        (claude-usage--install-mode-line)
        (claude-usage--start-timer))
    (claude-usage--stop-timer)
    (claude-usage--abandon-fetch)
    (claude-usage--remove-mode-line)
    (setq claude-usage-mode-line-string "")
    (force-mode-line-update t)))

(provide 'claude-usage)

;;; claude-usage.el ends here
