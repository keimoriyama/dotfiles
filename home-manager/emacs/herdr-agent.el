;;; herdr-agent.el --- agent-shell style buffers for herdr agents -*- lexical-binding: t; -*-

;;; Commentary:

;; Drives coding agents that run inside herdr (https://herdr.dev) from Emacs.
;; Each agent gets a buffer showing its terminal screen above an input area,
;; like agent-shell, and `herdr-agent-list' lists every live agent.
;;
;; herdr exposes terminals, not a structured protocol like ACP, so the output
;; side is a scrape of the agent's screen (`herdr agent read') with the
;; agent's own input box and footer trimmed off.  Everything goes through the
;; `herdr' CLI, run asynchronously so a long turn never blocks Emacs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(defgroup herdr-agent nil
  "Emacs front end for agents running in herdr."
  :group 'tools
  :prefix "herdr-agent-")

(defcustom herdr-agent-program "herdr"
  "Name or path of the herdr executable."
  :type 'string)

(defcustom herdr-agent-session nil
  "Named herdr session to talk to, or nil for the default session."
  :type '(choice (const :tag "Default session" nil) string))

(defcustom herdr-agent-read-lines 300
  "Number of screen lines requested when reading an agent's output."
  :type 'integer)

(defcustom herdr-agent-poll-interval 2
  "Seconds between polls of the agent list while herdr buffers are open."
  :type 'number)

(defcustom herdr-agent-default-kind "claude"
  "Agent kind offered first by `herdr-agent-start'."
  :type 'string)

(defcustom herdr-agent-kinds
  '("claude" "codex" "opencode" "gemini" "copilot" "cursor" "amp" "pi")
  "Agent kinds offered for completion by `herdr-agent-start'."
  :type '(repeat string))

(defcustom herdr-agent-screen-functions '(herdr-agent-trim-footer)
  "Functions applied in order to the screen text before it is displayed.
Each takes the screen text and returns the text to show."
  :type 'hook)

(defface herdr-agent-status-working '((t :inherit warning))
  "Face for agents that are working.")

(defface herdr-agent-status-blocked '((t :inherit error :weight bold))
  "Face for agents waiting at an approval or question dialog.")

(defface herdr-agent-status-done '((t :inherit success :weight bold))
  "Face for agents that finished a turn not yet looked at.")

(defface herdr-agent-status-idle '((t :inherit success))
  "Face for agents ready for input.")

(defface herdr-agent-status-other '((t :inherit shadow))
  "Face for agents whose state is unknown or gone.")

(defface herdr-agent-prompt '((t :inherit minibuffer-prompt))
  "Face for the prompt in front of the input area.")

(defconst herdr-agent--prompt-string "herdr> "
  "Text shown in front of the input area.")

;;;; CLI

(defun herdr-agent--process-environment ()
  "Return `process-environment' pointed at `herdr-agent-session'.
herdr prefers HERDR_SOCKET_PATH over HERDR_SESSION, and an Emacs started
inside a herdr pane inherits that variable, so it is unset whenever a
session is chosen explicitly."
  (if herdr-agent-session
      (append (list (concat "HERDR_SESSION=" herdr-agent-session)
                    "HERDR_SOCKET_PATH")
              process-environment)
    process-environment))

(defun herdr-agent--parse-json (text)
  "Parse the JSON object in TEXT into an alist, or return nil."
  (condition-case nil
      (json-parse-string (string-trim text)
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil)
    (json-error nil)))

(defun herdr-agent--error-code (text)
  "Return the herdr error code in CLI output TEXT, or nil."
  (alist-get 'code (alist-get 'error (herdr-agent--parse-json text))))

(defun herdr-agent--error-message (args text)
  "Describe the failure of herdr ARGS whose output was TEXT."
  (let ((err (alist-get 'error (herdr-agent--parse-json text))))
    (format "herdr %s: %s"
            (string-join (seq-take args 2) " ")
            (or (alist-get 'message err)
                (string-trim text)))))

(cl-defun herdr-agent--call (args &key on-success on-error raw)
  "Run herdr with ARGS asynchronously.
ON-SUCCESS receives the parsed `result' of the JSON reply, or the raw
output string when RAW is non-nil.  ON-ERROR receives the error code
and the output; without it failures are reported with `message'.
herdr prints errors as JSON on stderr, so stderr is merged into the
output buffer and the exit status tells the two apart."
  (let* ((process-environment (herdr-agent--process-environment))
         (output (generate-new-buffer " *herdr-agent*")))
    (make-process
     :name "herdr-agent"
     :buffer output
     :command (cons herdr-agent-program args)
     :connection-type 'pipe
     :noquery t
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         (let ((text (with-current-buffer output (buffer-string)))
               (status (process-exit-status process)))
           (kill-buffer output)
           (if (zerop status)
               (when on-success
                 (funcall on-success
                          (if raw
                              text
                            (alist-get 'result (herdr-agent--parse-json text)))))
             (if on-error
                 (funcall on-error (herdr-agent--error-code text) text)
               (message "%s" (herdr-agent--error-message args text))))))))))

;;;; Pure helpers

(defun herdr-agent-trim-footer (text)
  "Drop the agent's own input box and footer from screen TEXT.
TUI agents such as Claude Code draw their input box between two
horizontal rules at the bottom of the screen, followed by status lines.
Everything from the second-to-last rule down is removed.  Screens with
fewer than two rules are returned without trailing blank lines only."
  (let* ((lines (split-string text "\n"))
         (rules (cl-loop for line in lines
                         for i from 0
                         when (string-match-p "\\`[ \t]*─\\{8,\\}[ \t]*\\'" line)
                         collect i))
         (kept (if (>= (length rules) 2)
                   (seq-take lines (car (last rules 2)))
                 lines)))
    (string-trim-right (string-join kept "\n"))))

(defun herdr-agent--apply-screen-functions (text)
  "Run `herdr-agent-screen-functions' over TEXT."
  (seq-reduce (lambda (acc fn) (funcall fn acc))
              herdr-agent-screen-functions
              text))

(defun herdr-agent--display-name (agent)
  "Return a human readable name for AGENT, falling back to its pane id."
  (or (alist-get 'name agent)
      (format "%s@%s" (or (alist-get 'agent agent) "agent")
              (alist-get 'pane_id agent))))

(defun herdr-agent--buffer-name (agent)
  "Return the name of the buffer that shows AGENT."
  (format "*herdr: %s*" (herdr-agent--display-name agent)))

(defun herdr-agent--status-face (status)
  "Return the face used to show agent STATUS."
  (pcase status
    ("working" 'herdr-agent-status-working)
    ("blocked" 'herdr-agent-status-blocked)
    ("done" 'herdr-agent-status-done)
    ("idle" 'herdr-agent-status-idle)
    (_ 'herdr-agent-status-other)))

(defun herdr-agent--status-label (status)
  "Return agent STATUS as a propertized string."
  (let ((status (or status "gone")))
    (propertize status 'face (herdr-agent--status-face status))))

(defun herdr-agent--submit-commands (pane-id text)
  "Return the herdr argument lists that submit TEXT to the agent in PANE-ID.
`herdr agent prompt' delivers text as a bracketed paste, which Claude
Code then treats as pasted content rather than as the user's request,
so the text is typed with `pane send-text' and submitted with Enter."
  (unless (string-empty-p (string-trim text))
    (list (list "pane" "send-text" pane-id text)
          (list "pane" "send-keys" pane-id "enter"))))

(defun herdr-agent--list-entry (agent)
  "Return a `tabulated-list-entries' element describing AGENT."
  (list (alist-get 'pane_id agent)
        (vector (herdr-agent--display-name agent)
                (or (alist-get 'agent agent) "")
                (herdr-agent--status-label (alist-get 'agent_status agent))
                (alist-get 'pane_id agent)
                (abbreviate-file-name
                 (or (alist-get 'foreground_cwd agent)
                     (alist-get 'cwd agent)
                     "")))))

(defun herdr-agent--region-context (file start-line end-line text)
  "Build a prompt quoting TEXT from lines START-LINE..END-LINE of FILE."
  (format "%s:%d-%d\n```\n%s\n```\n"
          (or file "(buffer)") start-line end-line
          (string-trim-right text)))

;;;; Agent buffer

(defvar-local herdr-agent--pane-id nil
  "Pane id of the agent shown in this buffer.")

(defvar-local herdr-agent--info nil
  "Latest `agent list' record for this buffer's agent, or nil once gone.")

(defvar-local herdr-agent--screen nil
  "Screen text currently displayed, used to skip redundant redraws.")

(defvar-local herdr-agent--reading nil
  "Non-nil while a screen read for this buffer is in flight.")

(defvar-local herdr-agent--input-start nil
  "Marker at the start of the input area.")

(defvar-keymap herdr-agent-mode-map
  :doc "Keymap for `herdr-agent-mode'."
  "RET" #'herdr-agent-send-input
  "C-j" #'newline
  "C-c C-c" #'herdr-agent-send-input
  "C-c C-k" #'herdr-agent-send-keys
  "C-c C-i" #'herdr-agent-interrupt
  "C-c C-r" #'herdr-agent-refresh
  "C-c C-f" #'herdr-agent-focus
  "C-c C-l" #'herdr-agent-list)

;; Not derived from `text-mode': modeline packages hooked on
;; `text-mode-hook' run after this body and replace the header line.
(define-derived-mode herdr-agent-mode nil "herdr"
  "Major mode for talking to an agent running in herdr.
The upper part mirrors the agent's screen; type below the prompt and
press RET to submit.  \\<herdr-agent-mode-map>\\[herdr-agent-send-keys] \
answers approval dialogs with raw keys.

\\{herdr-agent-mode-map}"
  (setq-local header-line-format '(:eval (herdr-agent--header-line)))
  (setq-local truncate-lines nil)
  (add-hook 'kill-buffer-hook #'herdr-agent--maybe-stop-polling nil t))

(defun herdr-agent--header-line ()
  "Describe the agent of the current buffer for the header line."
  (let ((info herdr-agent--info))
    (format " %s  %s  %s  %s"
            (propertize (herdr-agent--display-name
                         (or info `((pane_id . ,herdr-agent--pane-id))))
                        'face 'bold)
            (or (alist-get 'agent info) "")
            (herdr-agent--status-label (alist-get 'agent_status info))
            (abbreviate-file-name
             (or (alist-get 'foreground_cwd info) (alist-get 'cwd info) "")))))

(defun herdr-agent--setup-buffer ()
  "Insert the empty screen area and the prompt into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize herdr-agent--prompt-string
                        'face 'herdr-agent-prompt
                        'read-only t
                        'field 'output
                        'rear-nonsticky t))
    (setq herdr-agent--input-start (point-marker))))

(defun herdr-agent--replace-screen (text)
  "Replace the displayed screen of the current buffer with TEXT.
Point stays in the input area when it was there, and windows that were
showing the end of the screen keep following it."
  (let* ((inhibit-read-only t)
         (prompt-start (- herdr-agent--input-start
                          (length herdr-agent--prompt-string)))
         (in-input (>= (point) herdr-agent--input-start))
         (line (line-number-at-pos))
         (followers (seq-filter
                     (lambda (window)
                       (>= (window-point window) prompt-start))
                     (get-buffer-window-list (current-buffer) nil t))))
    (save-excursion
      (goto-char (point-min))
      (delete-region (point-min) prompt-start)
      (insert (propertize (concat text "\n\n")
                          'read-only t
                          'field 'output
                          'front-sticky '(read-only)
                          'rear-nonsticky t)))
    (unless in-input
      (goto-char (point-min))
      (forward-line (1- line)))
    (dolist (window followers)
      (with-selected-window window
        (set-window-point window (max (window-point window)
                                      herdr-agent--input-start))
        (recenter -1)))
    (setq herdr-agent--screen text)))

(defun herdr-agent--read-screen (buffer)
  "Fetch the agent screen of BUFFER and redraw it when it changed."
  (with-current-buffer buffer
    (unless herdr-agent--reading
      (setq herdr-agent--reading t)
      (herdr-agent--call
       (list "agent" "read" herdr-agent--pane-id
             "--source" "recent-unwrapped"
             "--lines" (number-to-string herdr-agent-read-lines))
       :raw t
       :on-success
       (lambda (text)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq herdr-agent--reading nil)
             (let ((screen (herdr-agent--apply-screen-functions text)))
               (unless (equal screen herdr-agent--screen)
                 (herdr-agent--replace-screen screen))))))
       :on-error
       (lambda (_code _text)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq herdr-agent--reading nil))))))))

(defun herdr-agent--buffer-for (agent)
  "Return the buffer for AGENT, creating it when needed."
  (let ((pane-id (alist-get 'pane_id agent)))
    (or (herdr-agent--find-buffer pane-id)
        (with-current-buffer (generate-new-buffer
                              (herdr-agent--buffer-name agent))
          (herdr-agent-mode)
          (setq herdr-agent--pane-id pane-id
                herdr-agent--info agent)
          (herdr-agent--setup-buffer)
          (current-buffer)))))

(defun herdr-agent--find-buffer (pane-id)
  "Return the live agent buffer for PANE-ID, or nil."
  (seq-find (lambda (buffer)
              (equal (buffer-local-value 'herdr-agent--pane-id buffer)
                     pane-id))
            (herdr-agent--buffers)))

(defun herdr-agent--buffers ()
  "Return all live `herdr-agent-mode' buffers."
  (seq-filter (lambda (buffer)
                (eq (buffer-local-value 'major-mode buffer) 'herdr-agent-mode))
              (buffer-list)))

(defun herdr-agent--show (agent)
  "Pop to the buffer of AGENT and refresh its screen."
  (let ((buffer (herdr-agent--buffer-for agent)))
    (pop-to-buffer buffer)
    (goto-char (point-max))
    (herdr-agent--read-screen buffer)
    (herdr-agent--start-polling)
    buffer))

;;;; Polling

(defvar herdr-agent--timer nil
  "Timer polling herdr while agent buffers are open.")

(defvar herdr-agent--polling nil
  "Non-nil while an `agent list' request is in flight.")

(defun herdr-agent--start-polling ()
  "Start the poll timer unless it is running."
  (unless (timerp herdr-agent--timer)
    (setq herdr-agent--timer
          (run-with-timer herdr-agent-poll-interval herdr-agent-poll-interval
                          #'herdr-agent--poll))))

(defun herdr-agent--maybe-stop-polling ()
  "Stop the poll timer when no herdr buffer is left after this one."
  (unless (seq-some (lambda (buffer)
                      (and (not (eq buffer (current-buffer)))
                           (memq (buffer-local-value 'major-mode buffer)
                                 '(herdr-agent-mode herdr-agent-list-mode))))
                    (buffer-list))
    (when (timerp herdr-agent--timer)
      (cancel-timer herdr-agent--timer))
    (setq herdr-agent--timer nil)))

(defun herdr-agent--poll ()
  "Refresh agent states, and the screens of agents that changed or work."
  (unless herdr-agent--polling
    (setq herdr-agent--polling t)
    (herdr-agent--call
     '("agent" "list")
     :on-success
     (lambda (result)
       (setq herdr-agent--polling nil)
       (herdr-agent--apply-agents (alist-get 'agents result)))
     :on-error
     (lambda (_code _text)
       (setq herdr-agent--polling nil)))))

(defun herdr-agent--apply-agents (agents)
  "Update agent buffers and the list buffer from AGENTS."
  (dolist (buffer (herdr-agent--buffers))
    (with-current-buffer buffer
      (let* ((old (alist-get 'agent_status herdr-agent--info))
             (info (seq-find (lambda (agent)
                               (equal (alist-get 'pane_id agent)
                                      herdr-agent--pane-id))
                             agents))
             (new (alist-get 'agent_status info)))
        (setq herdr-agent--info info)
        (when (and info (or (not (equal old new)) (equal new "working")))
          (herdr-agent--read-screen buffer))
        (unless (equal old new)
          (force-mode-line-update)))))
  (when-let* ((list-buffer (get-buffer "*herdr agents*")))
    (with-current-buffer list-buffer
      (setq tabulated-list-entries (mapcar #'herdr-agent--list-entry agents))
      (tabulated-list-print t))))

;;;; Commands

(defun herdr-agent--current-pane-id ()
  "Return the pane id of the agent this command should act on."
  (or herdr-agent--pane-id
      (and (derived-mode-p 'herdr-agent-list-mode) (tabulated-list-get-id))
      (user-error "Not in a herdr agent buffer")))

(defun herdr-agent--submit (pane-id text)
  "Submit TEXT to the agent in PANE-ID unless it waits at a dialog."
  (herdr-agent--call
   (list "agent" "get" pane-id)
   :on-success
   (lambda (result)
     (let ((status (alist-get 'agent_status (alist-get 'agent result))))
       (if (equal status "blocked")
           (message "herdr: agent is blocked; answer it with herdr-agent-send-keys")
         (pcase-let ((`(,send ,enter) (herdr-agent--submit-commands pane-id text)))
           (herdr-agent--call
            send
            :on-success
            (lambda (_)
              (herdr-agent--call enter
                                 :on-success
                                 (lambda (_) (herdr-agent--poll)))))))))))

(defun herdr-agent-send-input ()
  "Submit the text of the input area to the agent."
  (interactive)
  (unless (and herdr-agent--input-start
               (>= (point) herdr-agent--input-start))
    (user-error "Point is not in the input area"))
  (let ((text (buffer-substring-no-properties herdr-agent--input-start
                                              (point-max))))
    (when (string-empty-p (string-trim text))
      (user-error "Nothing to send"))
    (delete-region herdr-agent--input-start (point-max))
    (herdr-agent--submit herdr-agent--pane-id text)))

(defun herdr-agent--read-agent (prompt)
  "Read a live agent with PROMPT and call back with its record."
  (let* ((result (herdr-agent--call-sync '("agent" "list")))
         (agents (alist-get 'agents result))
         (choices (mapcar (lambda (agent)
                            (cons (format "%s (%s, %s)"
                                          (herdr-agent--display-name agent)
                                          (alist-get 'agent agent)
                                          (alist-get 'agent_status agent))
                                  agent))
                          agents)))
    (unless choices
      (user-error "No agents are running in herdr"))
    (cdr (assoc (completing-read prompt choices nil t) choices))))

(defun herdr-agent--call-sync (args)
  "Run herdr with ARGS synchronously and return the parsed result.
Only used for quick lookups needed to build completion candidates."
  (let ((process-environment (herdr-agent--process-environment)))
    (with-temp-buffer
      (let ((status (apply #'call-process herdr-agent-program nil t nil args)))
        (unless (eql status 0)
          (user-error "%s" (herdr-agent--error-message args (buffer-string))))
        (alist-get 'result (herdr-agent--parse-json (buffer-string)))))))

;;;###autoload
(defun herdr-agent-open ()
  "Open the buffer of a live herdr agent."
  (interactive)
  (herdr-agent--show (herdr-agent--read-agent "Agent: ")))

;;;###autoload
(defun herdr-agent-send-region (start end)
  "Send the region between START and END to an agent, quoted with its location."
  (interactive "r")
  (let ((text (herdr-agent--region-context
               (and buffer-file-name (abbreviate-file-name buffer-file-name))
               (line-number-at-pos start) (line-number-at-pos end)
               (buffer-substring-no-properties start end)))
        (agent (herdr-agent--read-agent "Send region to: ")))
    (with-current-buffer (herdr-agent--show agent)
      (goto-char (point-max))
      (insert text))))

(defun herdr-agent-send-keys (keys)
  "Send KEYS, a space separated list such as \"down enter\", to the agent.
Use this to answer approval and question dialogs."
  (interactive (list (read-string "Keys (e.g. 1, down enter, esc): ")))
  (herdr-agent--call
   (append (list "agent" "send-keys" (herdr-agent--current-pane-id))
           (split-string keys))
   :on-success (lambda (_) (herdr-agent--poll))))

(defun herdr-agent-interrupt ()
  "Send Escape to the agent to interrupt its current turn."
  (interactive)
  (herdr-agent-send-keys "esc"))

(defun herdr-agent-refresh ()
  "Re-read the agent screen now."
  (interactive)
  (setq herdr-agent--screen nil)
  (herdr-agent--read-screen (current-buffer)))

(defun herdr-agent-focus ()
  "Focus the agent's pane in the herdr TUI."
  (interactive)
  (herdr-agent--call (list "agent" "focus" (herdr-agent--current-pane-id))))

;;;###autoload
(defun herdr-agent-start (name kind directory)
  "Start agent NAME of KIND in DIRECTORY inside a new herdr workspace.
A herdr server must already be running (start it with `herdr')."
  (interactive
   (list (read-string "Agent name: ")
         (completing-read "Kind: " herdr-agent-kinds nil nil nil nil
                          herdr-agent-default-kind)
         (read-directory-name "Directory: ")))
  (unless (string-match-p "\\`[a-z][a-z0-9_-]\\{0,31\\}\\'" name)
    (user-error "Agent names must match [a-z][a-z0-9_-]{0,31}"))
  (let ((directory (expand-file-name directory)))
    (herdr-agent--call
     (list "workspace" "create" "--cwd" directory "--label" name "--no-focus")
     :on-success
     (lambda (result)
       (let ((pane-id (alist-get 'pane_id (alist-get 'root_pane result))))
         (message "herdr: starting %s in %s..." name pane-id)
         (herdr-agent--call
          (list "agent" "start" name "--kind" kind "--pane" pane-id)
          :on-success
          (lambda (result) (herdr-agent--show (alist-get 'agent result)))
          :on-error
          (lambda (code text)
            ;; A startup dialog such as Claude Code's folder trust prompt
            ;; makes herdr report agent_not_ready, yet the agent is
            ;; registered and waits for an answer in its buffer.
            (if (equal code "agent_not_ready")
                (herdr-agent--show `((pane_id . ,pane-id) (name . ,name)
                                     (agent . ,kind)
                                     (agent_status . "blocked")))
              (message "%s" (herdr-agent--error-message
                             '("agent" "start") text))))))))))

;;;; Agent list

(defvar-keymap herdr-agent-list-mode-map
  :doc "Keymap for `herdr-agent-list-mode'."
  :parent tabulated-list-mode-map
  "RET" #'herdr-agent-list-open
  "k" #'herdr-agent-send-keys
  "f" #'herdr-agent-focus
  "s" #'herdr-agent-start)

(define-derived-mode herdr-agent-list-mode tabulated-list-mode "herdr agents"
  "Major mode listing the agents running in herdr."
  (setq tabulated-list-format [("Name" 20 t)
                               ("Kind" 10 t)
                               ("Status" 9 t)
                               ("Pane" 8 t)
                               ("Directory" 0 t)])
  (tabulated-list-init-header)
  (add-hook 'kill-buffer-hook #'herdr-agent--maybe-stop-polling nil t))

(defun herdr-agent-list-open ()
  "Open the buffer of the agent at point."
  (interactive)
  (let ((pane-id (or (tabulated-list-get-id) (user-error "No agent here"))))
    (herdr-agent--show
     (or (seq-find (lambda (agent) (equal (alist-get 'pane_id agent) pane-id))
                   (alist-get 'agents (herdr-agent--call-sync '("agent" "list"))))
         (user-error "Agent in %s is gone" pane-id)))))

;;;###autoload
(defun herdr-agent-list ()
  "List the agents running in herdr."
  (interactive)
  (with-current-buffer (get-buffer-create "*herdr agents*")
    (herdr-agent-list-mode)
    (pop-to-buffer (current-buffer))
    (herdr-agent--poll)
    (herdr-agent--start-polling)))

(provide 'herdr-agent)
;;; herdr-agent.el ends here
