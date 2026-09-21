;;; agent-usage-format-tests.el --- Tests for agent-usage-format -*- lexical-binding: t; -*-

(require 'ert)

(load (expand-file-name "agent-usage-format.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest agent-usage-format-shows-percent-and-reset-countdown ()
  "Windows show the spent share, a countdown, and a placeholder when unknown."
  (should
   (equal
    (substring-no-properties
     (agent-usage-format-windows
      '((:label "5h" :used 95 :reset 4600)
        (:label "7d" :used nil :reset nil))
      1000))
    " [5h 95%↻1h0m · 7d --%↻-]"))
  (should
   (equal
    (substring-no-properties
     (agent-usage-format-windows '((:label "5h" :used 10 :reset 999)) 1000))
    " [5h 10%↻now]"))
  (should-not (agent-usage-format-windows nil 1000)))

(ert-deftest agent-usage-format-keeps-the-countdown-below-the-window-length ()
  "A five-hour window with 4h7m left must not read as a rounded-up 5h."
  (should (equal " [5h 64%↻4h7m]"
                 (substring-no-properties
                  (agent-usage-format-windows
                   '((:label "5h" :used 64 :reset 14820)) 0)))))

(ert-deftest agent-usage-format-scales-countdown-units ()
  "The countdown steps from minutes through hours to days."
  (should (equal " [5h 1%↻30m]"
                 (substring-no-properties
                  (agent-usage-format-windows
                   '((:label "5h" :used 1 :reset 1800)) 0))))
  (should (equal " [5h 1%↻5h0m]"
                 (substring-no-properties
                  (agent-usage-format-windows
                   '((:label "5h" :used 1 :reset 18000)) 0))))
  (should (equal " [7d 1%↻1d4h]"
                 (substring-no-properties
                  (agent-usage-format-windows
                   '((:label "7d" :used 1 :reset 100800)) 0)))))

(ert-deftest agent-usage-format-reads-millisecond-resets ()
  "An epoch reported in milliseconds counts down like the same epoch in seconds."
  (let ((seconds 1788786599)
        (now 1788768599))
    (should (equal (substring-no-properties
                    (agent-usage-format-windows
                     `((:label "5h" :used 1 :reset ,(* seconds 1000))) now))
                   (substring-no-properties
                    (agent-usage-format-windows
                     `((:label "5h" :used 1 :reset ,seconds)) now))))))

(ert-deftest agent-usage-format-warns-as-the-quota-fills ()
  "Colour tracks how much of the window is spent."
  ;; Index 5 is the first digit of the percentage in " [5h NN%↻-]".
  (should (eq 'success
              (get-text-property
               5 'face (agent-usage-format-windows
                        '((:label "5h" :used 69 :reset nil)) 0))))
  (should (eq 'warning
              (get-text-property
               5 'face (agent-usage-format-windows
                        '((:label "5h" :used 70 :reset nil)) 0))))
  (should (eq 'error
              (get-text-property
               5 'face (agent-usage-format-windows
                        '((:label "5h" :used 90 :reset nil)) 0)))))

(ert-deftest agent-usage-format-appends-a-detail-to-the-percentage ()
  "A window may carry an absolute figure next to its share."
  (should (equal " [Org 61% $132/$215↻1h0m]"
                 (substring-no-properties
                  (agent-usage-format-windows
                   '((:label "Org" :used 61 :detail " $132/$215" :reset 4600))
                   1000))))
  (should (equal " [Org 61%↻1h0m]"
                 (substring-no-properties
                  (agent-usage-format-windows
                   '((:label "Org" :used 61 :detail nil :reset 4600))
                   1000)))))

(provide 'agent-usage-format-tests)
;;; agent-usage-format-tests.el ends here
