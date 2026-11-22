;;; marimo-table.el --- Cursor-driven mo.ui.table results in Org -*- lexical-binding: t; -*-

;;; Commentary:
;; A cell whose whole output is one mo.ui.table renders as an Org table.
;; RET on a row selects it through the kernel; [ and ] change pages.

;;; Code:
(require 'cl-lib)
(require 'dom)

(defvar marimo--connection)
(defvar marimo--session)
(defvar marimo--outputs)
(declare-function marimo--uri "marimo")
(declare-function marimo--render "marimo")
(declare-function jsonrpc-async-request "jsonrpc")

(defcustom marimo-table-column-width 40
  "Maximum display width of one table column."
  :group 'marimo :type 'natnum)

(defvar-local marimo-table--tables nil
  "Hash table from UI element object ID to table state plist.")
(defvar-local marimo-table--calls nil
  "Alist from pending function call ID to UI element object ID.")
(defvar marimo-table--call-counter 0)

(defvar marimo-table-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'marimo-table-select)
    (define-key map (kbd "]") #'marimo-table-next-page)
    (define-key map (kbd "[") #'marimo-table-previous-page)
    map)
  "Keymap active on rendered table results.")

(defun marimo-table--json (text &optional object-type)
  "Parse JSON TEXT, returning objects as OBJECT-TYPE (default plist)."
  (json-parse-string text :object-type (or object-type 'plist) :array-type 'list
                     :null-object nil :false-object "false"))

(defun marimo-table--rows (data)
  "Decode the JSON-encoded row string DATA, or nil for non-inline data."
  (and (stringp data) (string-prefix-p "[" data)
       (marimo-table--json data 'alist)))

(defun marimo-table--parse (output)
  "Return a table state plist when OUTPUT is exactly one inline mo.ui.table."
  (let ((data (plist-get output :data)))
    (when (and (equal (plist-get output :mimetype) "text/html") (stringp data)
               (string-match-p "\\`<marimo-ui-element [^>]*><marimo-table " data)
               (string-suffix-p "</marimo-table></marimo-ui-element>" data)
               (fboundp 'libxml-parse-html-region))
      (let* ((dom (with-temp-buffer
                    (insert data)
                    (libxml-parse-html-region (point-min) (point-max))))
             (elements (dom-by-tag dom 'marimo-ui-element))
             (table (car (dom-by-tag dom 'marimo-table)))
             (attribute (lambda (name)
                          (let ((value (dom-attr table name)))
                            (and value (marimo-table--json value)))))
             (selection (funcall attribute 'data-selection))
             (rows (marimo-table--rows (funcall attribute 'data-data))))
        ;; LIMITATION: lazy or file-backed data and cell selection keep using
        ;; the official renderer.
        (when (and (= (length elements) 1) table rows
                   (member selection '(nil "single" "multi")))
          (list :object-id (dom-attr (car elements) 'object-id)
                :random-id (dom-attr (car elements) 'random-id)
                :label (let ((label (funcall attribute 'data-label)))
                         (and (stringp label)
                              (string-trim
                               (dom-texts (with-temp-buffer
                                            (insert label)
                                            (libxml-parse-html-region (point-min) (point-max)))
                                          ""))))
                :rows rows
                :page 0
                :page-size (funcall attribute 'data-page-size)
                :total (funcall attribute 'data-total-rows)
                :selection selection
                :selected (mapcar (lambda (v) (format "%s" v))
                                  (funcall attribute 'data-initial-value))))))))

(defun marimo-table-state (id output)
  "Return cell ID's table state for OUTPUT, keeping page and selection."
  (when-let* ((parsed (marimo-table--parse output)))
    (unless marimo-table--tables
      (setq marimo-table--tables (make-hash-table :test #'equal)))
    (let* ((key (plist-get parsed :object-id))
           (existing (gethash key marimo-table--tables)))
      (if (and existing (equal (plist-get existing :random-id) (plist-get parsed :random-id)))
          existing
        (puthash key (plist-put parsed :cell id) marimo-table--tables)))))

(defun marimo-table--row-id (table index row)
  "Return the selection ID of ROW at INDEX on TABLE's current page."
  (let ((stable (assq '_marimo_row_id row)))
    (format "%s" (if stable (cdr stable)
                   (+ index (* (plist-get table :page) (or (plist-get table :page-size) 0)))))))

(defun marimo-table--cell (value)
  "Return VALUE as single-line text that cannot break the Org table."
  (truncate-string-to-width
   (replace-regexp-in-string "[|\n\r\t]" " " (cond ((stringp value) value)
                                                   ((null value) "")
                                                   ((eq value t) "true")
                                                   (t (format "%s" value))))
   marimo-table-column-width nil nil "…"))

(defun marimo-table-text (table)
  "Return Org text for TABLE with cursor bindings on every line."
  (let* ((object-id (plist-get table :object-id))
         (rows (plist-get table :rows))
         (columns (cl-remove '_marimo_row_id
                             (cl-remove-duplicates (mapcan (lambda (row) (mapcar #'car row)) rows)
                                                   :from-end t)))
         (cells (mapcar (lambda (row) (mapcar (lambda (c) (marimo-table--cell (alist-get c row))) columns))
                        rows))
         (header (mapcar (lambda (c) (marimo-table--cell (symbol-name c))) columns))
         (widths (apply #'cl-mapcar (lambda (&rest column) (apply #'max 1 (mapcar #'string-width column)))
                        header cells))
         (line (lambda (mark values)
                 (concat "| " mark " | "
                         (mapconcat #'identity
                                    (cl-mapcar (lambda (v w) (concat v (make-string (- w (string-width v)) ?\s)))
                                               values widths)
                                    " | ")
                         " |\n")))
         (page-size (plist-get table :page-size))
         (total (plist-get table :total))
         (pages (if (and (numberp total) page-size (> page-size 0))
                    (max 1 (ceiling total (float page-size))) 1))
         (propertize-line (lambda (text row-id)
                            (propertize text 'marimo-table (cons object-id row-id)
                                        'keymap marimo-table-map))))
    (concat
     (when (plist-get table :label)
       (funcall propertize-line (concat ": " (plist-get table :label) "\n") nil))
     (funcall propertize-line (funcall line " " header) nil)
     (funcall propertize-line
              (concat "|---+" (mapconcat (lambda (w) (make-string (+ w 2) ?-)) widths "+") "|\n") nil)
     (apply #'concat
            (cl-loop for row in rows for values in cells for index from 0
                     collect (let ((row-id (marimo-table--row-id table index row)))
                               (funcall propertize-line
                                        (funcall line (if (member row-id (plist-get table :selected)) "*" " ")
                                                 values)
                                        row-id))))
     (funcall propertize-line
              (format ": page %d/%d, %s rows%s\n" (1+ (plist-get table :page)) pages total
                      (if (plist-get table :selection) " (RET: select, [ ]: page)" " ([ ]: page)"))
              nil))))

(defun marimo-table--at-point ()
  "Return (TABLE . ROW-ID) for the table result at point."
  (let* ((target (or (get-text-property (point) 'marimo-table)
                     (user-error "Point is not on a marimo table")))
         (table (and marimo-table--tables (gethash (car target) marimo-table--tables))))
    (unless table (user-error "Table state is unavailable; rerun the cell"))
    (cons table (cdr target))))

(defun marimo-table--command (command)
  "Send kernel COMMAND for this notebook's live session."
  (unless marimo--session (user-error "No running marimo kernel"))
  (jsonrpc-async-request marimo--connection 'marimo/command
                         (append command (list :notebookUri (marimo--uri)
                                               :kernelSessionId marimo--session))
                         :success-fn #'ignore
                         :error-fn (lambda (err) (message "Marimo table: %S" err))))

(defun marimo-table--refresh (table)
  "Render TABLE's cell again, keeping point on the same line."
  (let ((line (line-number-at-pos)) (column (current-column))
        (id (plist-get table :cell)))
    (marimo--render id (gethash id marimo--outputs))
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)))

(defun marimo-table-select ()
  "Toggle selection of the table row at point in the kernel."
  (interactive)
  (pcase-let* ((`(,table . ,row-id) (marimo-table--at-point))
               (mode (plist-get table :selection))
               (selected (plist-get table :selected)))
    (unless row-id (user-error "Point must be on a table row"))
    (unless mode (user-error "This table does not allow selection"))
    (let ((value (cond ((member row-id selected) (remove row-id selected))
                       ((equal mode "multi") (append selected (list row-id)))
                       (t (list row-id)))))
      (marimo-table--command (list :kind "update-ui-element"
                                   :objectIds (vector (plist-get table :object-id))
                                   :values (vector (vconcat value))))
      (plist-put table :selected value)
      (marimo-table--refresh table))))

(defun marimo-table--goto-page (delta)
  "Request the page DELTA pages away from the current one."
  (let* ((table (car (marimo-table--at-point)))
         (page-size (plist-get table :page-size))
         (total (plist-get table :total))
         (last (if (and (numberp total) page-size (> page-size 0))
                   (max 0 (1- (ceiling total (float page-size)))) 0))
         (page (+ (plist-get table :page) delta))
         (call (format "emacs-table-%d" (cl-incf marimo-table--call-counter))))
    (unless (<= 0 page last) (user-error "No more pages"))
    (push (list call (plist-get table :object-id) page) marimo-table--calls)
    (marimo-table--command (list :kind "invoke-function" :functionCallId call
                                 :namespace (plist-get table :object-id)
                                 :functionName "search"
                                 :args (list :page_size page-size :page_number page)))))

(defun marimo-table-next-page ()
  "Show the next page of the table at point."
  (interactive)
  (marimo-table--goto-page 1))

(defun marimo-table-previous-page ()
  "Show the previous page of the table at point."
  (interactive)
  (marimo-table--goto-page -1))

(defun marimo-table-function-result (notification)
  "Apply a kernel function-call-result NOTIFICATION to a pending page request."
  (when-let* ((call (assoc (plist-get notification :function_call_id) marimo-table--calls)))
    (setq marimo-table--calls (delq call marimo-table--calls))
    (pcase-let* ((`(,_ ,object-id ,page) call)
                 (table (and marimo-table--tables (gethash object-id marimo-table--tables)))
                 (result (plist-get notification :return_value))
                 (rows (marimo-table--rows (plist-get result :data))))
      (cond
       ((not (equal (plist-get (plist-get notification :status) :code) "ok"))
        (message "Marimo table: %s" (or (plist-get (plist-get notification :status) :message)
                                        "page request failed")))
       ((and table rows)
        (plist-put table :rows rows)
        (plist-put table :page page)
        (when (plist-get result :total_rows) (plist-put table :total (plist-get result :total_rows)))
        (marimo-table--refresh table))))))

(provide 'marimo-table)
;;; marimo-table.el ends here
