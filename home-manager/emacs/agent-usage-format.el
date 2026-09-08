;;; agent-usage-format.el --- Shared rate-limit mode-line format -*- lexical-binding: t; -*-

;;; Commentary:

;; One format for every provider's time-window rate limits, so Claude and
;; Codex read the same way in the mode line:
;;
;;     [5h 29%↻4h7m · 7d 15%↻6d3h]
;;
;; Each window is the spent share of its quota, followed by how long until
;; that window resets.  Callers embedding the result through `:eval' must
;; double its percent signs; those going through symbol indirection must not.

;;; Code:

(require 'subr-x)

(defun agent-usage-format--countdown (reset now)
  "Return how long until RESET, relative to NOW.
Two units, so a window reports the time it has actually got left
rather than a figure rounded up into its own label."
  (let* ((seconds (when (numberp reset)
                    ;; Some providers report milliseconds.
                    (if (> reset 100000000000) (/ reset 1000.0) reset)))
         (remaining (when seconds (- seconds now))))
    (cond
     ((not remaining) "-")
     ((< remaining 60) "now")
     ((< remaining 3600) (format "%dm" (floor remaining 60)))
     ((< remaining 86400) (format "%dh%dm"
                                  (floor remaining 3600)
                                  (% (floor remaining 60) 60)))
     (t (format "%dd%dh"
                (floor remaining 86400)
                (% (floor remaining 3600) 24))))))

(defun agent-usage-format-windows (windows &optional now)
  "Format WINDOWS for the mode line relative to NOW.
WINDOWS is a list of plists holding :label, :used and :reset, where
:used is the spent percentage and :reset an epoch time."
  (when windows
    (let ((now (or now (float-time))))
      (concat
       " ["
       (string-join
        (mapcar
         (lambda (window)
           (let* ((used (plist-get window :used))
                  (face (cond ((and used (>= used 90)) 'error)
                              ((and used (>= used 70)) 'warning)
                              (t 'success))))
             (format "%s %s↻%s"
                     (plist-get window :label)
                     (propertize (if used (format "%d%%" used) "--%")
                                 'face face)
                     (agent-usage-format--countdown
                      (plist-get window :reset) now))))
         windows)
        " · ")
       "]"))))

(provide 'agent-usage-format)
;;; agent-usage-format.el ends here
