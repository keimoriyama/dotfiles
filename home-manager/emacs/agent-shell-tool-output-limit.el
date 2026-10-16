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

(defun my-agent-shell-tool-output-limit-lines (output)
  "Return OUTPUT with the lines between its head and tail omitted."
  (let* ((lines (split-string output "\n"))
         (count (length lines))
         (head my-agent-shell-tool-output-head-lines)
         (tail my-agent-shell-tool-output-tail-lines))
    (if (<= count (+ head tail))
        output
      (concat (string-join (seq-take lines head) "\n")
              (format "\n\n... [%d lines omitted] ...\n\n" (- count head tail))
              (string-join (last lines tail) "\n")))))

(defun my-agent-shell-tool-output-limit-setup ()
  "Cap the line count of tool output that agent-shell displays."
  (advice-add 'agent-shell--truncate-tool-output-lines
              :filter-return #'my-agent-shell-tool-output-limit-lines))

(provide 'agent-shell-tool-output-limit)
;;; agent-shell-tool-output-limit.el ends here
