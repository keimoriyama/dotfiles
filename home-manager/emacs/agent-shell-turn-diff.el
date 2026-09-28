;;; agent-shell-turn-diff.el --- Show what the last agent-shell turn changed -*- lexical-binding: t; -*-

;;; Commentary:

;; Records the working tree as a git tree object when a prompt is submitted
;; and again when the turn completes, so the files the agent touched during
;; that turn can be shown as one diff.  Snapshots go through a throwaway
;; index, leaving HEAD, the real index and the working tree alone, and they
;; include untracked files.  Comparing trees rather than collecting ACP tool
;; call diffs also catches edits made through shell commands.
;;
;; LIMITATION: edits made by anyone else in the same repository while the
;; turn runs (the user, another shell) land in the same diff.
;; LIMITATION: snapshots run synchronously, so a very large repository
;; pauses Emacs briefly on submit and on turn completion.

(require 'map)
(require 'subr-x)

(defvar-local my-agent-shell-turn-diff--before nil
  "Tree recorded when the running turn's prompt was submitted.")

(defvar-local my-agent-shell-turn-diff--prompt nil
  "Prompt that started the running turn.")

(defvar-local my-agent-shell-turn-diff--last nil
  "Plist (:before :after :prompt) of the latest turn that changed files.")

(defvar my-agent-shell-turn-diff--last-buffer nil
  "Shell buffer whose latest turn changed files most recently.")

(defun my-agent-shell-turn-diff--git (&rest args)
  "Run git with ARGS in `default-directory'; return trimmed stdout or nil."
  (with-temp-buffer
    (when (zerop (apply #'process-file "git" nil '(t nil) nil args))
      (string-trim (buffer-string)))))

(defun my-agent-shell-turn-diff--snapshot ()
  "Return a git tree hash of the working tree, or nil outside a repository."
  (when-let* ((index (my-agent-shell-turn-diff--git
                      "rev-parse" "--path-format=absolute" "--git-path" "index")))
    (let* ((index-file (concat (file-remote-p default-directory) index))
           (temp (make-nearby-temp-file "agent-shell-turn-diff-index"))
           (process-environment
            (cons (concat "GIT_INDEX_FILE=" (file-local-name temp))
                  process-environment)))
      (unwind-protect
          (progn
            ;; Starting from the real index lets git skip rehashing files
            ;; whose stat is unchanged.  An empty file is not a valid index.
            (if (file-exists-p index-file)
                (copy-file index-file temp t)
              (delete-file temp))
            (and (my-agent-shell-turn-diff--git "add" "-A")
                 (my-agent-shell-turn-diff--git "write-tree")))
        (when (file-exists-p temp)
          (delete-file temp))))))

(defun my-agent-shell-turn-diff--on-submit (event)
  "Record the tree before the turn started by EVENT."
  ;; Input steered into a running turn belongs to that turn.
  (unless my-agent-shell-turn-diff--before
    (setq my-agent-shell-turn-diff--before (my-agent-shell-turn-diff--snapshot)
          my-agent-shell-turn-diff--prompt (map-nested-elt event '(:data :prompt)))))

(defun my-agent-shell-turn-diff--on-complete (_event)
  "Remember the finished turn if it changed any file."
  (when-let* ((before (prog1 my-agent-shell-turn-diff--before
                        (setq my-agent-shell-turn-diff--before nil)))
              (after (my-agent-shell-turn-diff--snapshot))
              ((not (equal before after))))
    (setq my-agent-shell-turn-diff--last
          (list :before before :after after
                :prompt my-agent-shell-turn-diff--prompt)
          my-agent-shell-turn-diff--last-buffer (current-buffer))
    (message "agent-shell: このターンでファイルが変更されました (%s)"
             (substitute-command-keys
              "\\[my-agent-shell-turn-diff-show]"))))

(defun my-agent-shell-turn-diff-setup ()
  "Track per-turn file changes in this agent-shell buffer."
  (let ((buffer (current-buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buffer
     :event 'input-submitted
     :on-event (lambda (event)
                 (with-current-buffer buffer
                   (my-agent-shell-turn-diff--on-submit event))))
    (agent-shell-subscribe-to
     :shell-buffer buffer
     :event 'turn-complete
     :on-event (lambda (event)
                 (with-current-buffer buffer
                   (my-agent-shell-turn-diff--on-complete event))))))

(defun my-agent-shell-turn-diff--render (before after)
  "Return a `diff-mode' buffer showing the change from BEFORE to AFTER."
  (let ((toplevel (my-agent-shell-turn-diff--git "rev-parse" "--show-toplevel"))
        (buffer (get-buffer-create "*agent-shell-turn-diff*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Visiting hunks resolves a/ and b/ paths against the toplevel.
        (setq default-directory
              (file-name-as-directory
               (concat (file-remote-p default-directory) toplevel)))
        (process-file "git" nil t nil "diff" "--no-color" before after))
      (goto-char (point-min))
      (diff-mode)
      (setq buffer-read-only t))
    buffer))

(defun my-agent-shell-turn-diff-show ()
  "Show the files the latest turn changed.
Inside an agent-shell buffer, use that shell's latest turn; elsewhere,
the most recent turn that changed files in any shell."
  (interactive)
  (let ((shell (if (derived-mode-p 'agent-shell-mode)
                   (current-buffer)
                 my-agent-shell-turn-diff--last-buffer)))
    (unless (and (buffer-live-p shell)
                 (buffer-local-value 'my-agent-shell-turn-diff--last shell))
      (user-error "ファイルを変更したターンがまだありません"))
    (with-current-buffer shell
      (let* ((last my-agent-shell-turn-diff--last)
             (buffer (my-agent-shell-turn-diff--render
                      (plist-get last :before) (plist-get last :after))))
        (with-current-buffer buffer
          (setq header-line-format
                (when-let* ((prompt (plist-get last :prompt)))
                  (string-replace "%" "%%" (car (split-string prompt "\n" t))))))
        (pop-to-buffer buffer)))))

(provide 'agent-shell-turn-diff)
;;; agent-shell-turn-diff.el ends here
