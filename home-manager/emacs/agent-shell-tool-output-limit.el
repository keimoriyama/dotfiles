;;; agent-shell-tool-output-limit.el --- Cap the line count of agent-shell tool output -*- lexical-binding: t; -*-

;;; Commentary:

;; A large file read or a whole-file diff from a write lands in the shell
;; buffer as is, and rendering its markdown and appending it to the
;; transcript freezes Emacs.  agent-shell only shortens overlong lines, so
;; this also keeps the head and tail of the output and drops the middle.
;; The tail is kept so a closing code fence survives.
;;
;; LIMITATION: the transcript stores the shortened output too.

(require 'seq)
(require 'subr-x)

(defvar my-agent-shell-tool-output-head-lines 150
  "Number of leading tool output lines kept.")

(defvar my-agent-shell-tool-output-tail-lines 50
  "Number of trailing tool output lines kept.")

(defun my-agent-shell-tool-output--open-fence (lines)
  "Return the backticks of the code fence LINES leave open, or nil.
A fence closes only on a run at least as long as the one that opened it,
so shorter runs inside, as in a markdown file shown in a longer fence,
stay content."
  (let (open)
    (dolist (line lines open)
      (when (string-match "\\` \\{0,3\\}\\(`\\{3,\\}\\)" line)
        (let ((run (match-string 1 line)))
          (cond ((not open) (setq open run))
                ((and (>= (length run) (length open))
                      (string-blank-p (substring line (match-end 0))))
                 (setq open nil))))))))

(defun my-agent-shell-tool-output-limit-lines (output)
  "Return OUTPUT with the lines between its head and tail omitted.
When the omitted lines close or open a code fence, matching fence lines
follow the omission note so the tail renders as it would have in full."
  (let* ((lines (split-string output "\n"))
         (count (length lines))
         (head my-agent-shell-tool-output-head-lines)
         (tail my-agent-shell-tool-output-tail-lines))
    (if (<= count (+ head tail))
        output
      (let* ((kept-head (seq-take lines head))
             (open-at-cut (my-agent-shell-tool-output--open-fence kept-head))
             (open-at-tail (my-agent-shell-tool-output--open-fence
                            (seq-take lines (- count tail))))
             (changed (not (equal open-at-cut open-at-tail))))
        (concat (string-join kept-head "\n")
                (format "\n\n... [%d lines omitted] ...\n\n" (- count head tail))
                (when (and changed open-at-cut) (concat open-at-cut "\n"))
                (when (and changed open-at-tail) (concat open-at-tail "\n"))
                (string-join (last lines tail) "\n"))))))

(defun my-agent-shell-tool-output-limit-setup ()
  "Cap the line count of tool output that agent-shell displays."
  (advice-add 'agent-shell--truncate-tool-output-lines
              :filter-return #'my-agent-shell-tool-output-limit-lines))

(provide 'agent-shell-tool-output-limit)
;;; agent-shell-tool-output-limit.el ends here
