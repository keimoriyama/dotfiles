;;; claude-usage.el --- Claude subscription usage in the mode line -*- lexical-binding: t; -*-

;;; Commentary:

;; Display Claude subscription usage in the mode line by polling the
;; `claude-usage-line' CLI asynchronously:
;;
;;     Claude 5h 68% | 7d 53%
;;
;; The values are the remaining quota of the 5-hour and 7-day windows,
;; i.e. 100 minus the `used_percentage' reported by the CLI.  Enable with
;; (claude-usage-mode 1).

;;; Code:

(require 'json)
(require 'subr-x)

(defgroup claude-usage nil
  "Claude subscription usage in the mode line."
  :group 'mode-line)

(defcustom claude-usage-command
  '("claude-usage-line" "--json")
  "Command used to retrieve Claude usage.
Claude Code hands the per-window percentages only to its status line, so
they are read back from the tool installed as that status line."
  :type '(repeat string))

(defcustom claude-usage-refresh-interval
  60
  "Refresh interval in seconds."
  :type 'integer)

(defvar claude-usage--state
  '(:five-hour nil
    :seven-day nil
    :five-hour-reset nil
    :seven-day-reset nil
    :updated-at nil
    :error nil)
  "Latest known usage.
:five-hour and :seven-day hold the remaining percentages,
:updated-at the time of the last successful fetch.")

(defvar claude-usage--timer nil)
(defvar claude-usage--process nil)

(defvar claude-usage-mode-line-string ""
  "Mode-line construct showing Claude usage.")

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
        (updated-at (plist-get claude-usage--state :updated-at))
        (error-message (plist-get claude-usage--state :error)))
    (message "Claude usage\n\n5-hour remaining: %s\n7-day remaining: %s\nLast updated: %s\nStatus: %s%s"
             (if five-hour (format "%d%%" five-hour) "?")
             (if seven-day (format "%d%%" seven-day) "?")
             (if updated-at (format-time-string "%Y-%m-%d %H:%M" updated-at) "never")
             (if error-message "ERROR" "OK")
             (if error-message (concat "\nError: " error-message) ""))))

(defun claude-usage--fetch ()
  "Start an asynchronous usage fetch unless one is already running."
  (unless (process-live-p claude-usage--process)
    (let ((buffer (generate-new-buffer " *claude-usage*")))
      (condition-case err
          (setq claude-usage--process
                (make-process
                 :name "claude-usage"
                 :buffer buffer
                 :command claude-usage-command
                 :noquery t
                 :sentinel #'claude-usage--process-sentinel))
        (error
         ;; Typically `claude-usage' is not on PATH.
         (kill-buffer buffer)
         (claude-usage--record-error (error-message-string err)))))))

(defun claude-usage--process-sentinel (process _event)
  "Consume the output of PROCESS once it has exited."
  (when (memq (process-status process) '(exit signal))
    (unwind-protect
        (cond
         ((and (eq (process-status process) 'exit)
               (zerop (process-exit-status process)))
          (claude-usage--handle-output process))
         ((eq (process-status process) 'exit)
          (claude-usage--record-error
           (format "%s exited with status %d"
                   (car claude-usage-command)
                   (process-exit-status process))))
         (t
          (claude-usage--record-error
           (format "%s was killed by a signal" (car claude-usage-command)))))
      (when (buffer-live-p (process-buffer process))
        (kill-buffer (process-buffer process)))
      (when (eq process claude-usage--process)
        (setq claude-usage--process nil)))))

(defun claude-usage--handle-output (process)
  "Install the usage reported in PROCESS's buffer, or record the failure."
  (let ((output (with-current-buffer (process-buffer process)
                  (buffer-string))))
    (if (string-blank-p output)
        (claude-usage--record-error "claude-usage produced no output")
      (condition-case err
          (progn
            (setq claude-usage--state (claude-usage--parse output))
            (claude-usage--update-mode-line))
        (error (claude-usage--record-error (error-message-string err)))))))

(defun claude-usage--record-error (message)
  "Record MESSAGE in the state and refresh the mode line."
  (plist-put claude-usage--state :error message)
  (claude-usage--update-mode-line))

(defun claude-usage--parse (output)
  "Parse JSON OUTPUT from `claude-usage-command' into a fresh state plist.
Signal an error on malformed input; percentages become remaining quota."
  (let* ((data (json-parse-string output :object-type 'alist :array-type 'list))
         (five-hour (alist-get 'five_hour data))
         (seven-day (alist-get 'seven_day data))
         ;; The tool reports how much of each window is spent as
         ;; `utilization_pct'.
         (five-used (alist-get 'utilization_pct five-hour))
         (seven-used (alist-get 'utilization_pct seven-day)))
    (unless five-hour
      (error "Output is missing five_hour"))
    (unless seven-day
      (error "Output is missing seven_day"))
    (unless (numberp five-used)
      (error "five_hour.utilization_pct is not a number"))
    (unless (numberp seven-used)
      (error "seven_day.utilization_pct is not a number"))
    (list :five-hour (round (- 100 five-used))
          :seven-day (round (- 100 seven-used))
          :five-hour-reset (alist-get 'resets_at five-hour)
          :seven-day-reset (alist-get 'resets_at seven-day)
          :updated-at (current-time)
          :error nil)))

(defun claude-usage--update-mode-line ()
  "Recompute `claude-usage-mode-line-string' from the current state."
  (setq claude-usage-mode-line-string
        (claude-usage--format-mode-line claude-usage--state))
  (force-mode-line-update t))

(defun claude-usage--format-mode-line (state)
  "Return the mode-line text for STATE.
Mode-line strings are decoded as constructs, so percent signs in the
text have to be doubled to survive rendering."
  (string-replace
   "%" "%%"
   (let ((five-hour (plist-get state :five-hour))
         (seven-day (plist-get state :seven-day)))
     (if (and five-hour seven-day)
         (format " Claude 5h %d%% | 7d %d%%%s"
                 five-hour
                 seven-day
                 (if (plist-get state :error) "*" ""))
       " Claude ?"))))

(defun claude-usage--start-timer ()
  (claude-usage--stop-timer)
  (setq claude-usage--timer
        (run-at-time 0 claude-usage-refresh-interval #'claude-usage-refresh)))

(defun claude-usage--stop-timer ()
  (when claude-usage--timer
    (cancel-timer claude-usage--timer)
    (setq claude-usage--timer nil)))

;;;###autoload
(define-minor-mode claude-usage-mode
  "Show Claude subscription usage in the mode line."
  :global t
  :group 'claude-usage
  (if claude-usage-mode
      (progn
        (unless (memq 'claude-usage-mode-line-string global-mode-string)
          (setq global-mode-string
                (append global-mode-string '(claude-usage-mode-line-string))))
        (claude-usage--start-timer))
    (claude-usage--stop-timer)
    (setq global-mode-string
          (delq 'claude-usage-mode-line-string global-mode-string))
    (setq claude-usage-mode-line-string "")
    (force-mode-line-update t)))

(provide 'claude-usage)

;;; claude-usage.el ends here
