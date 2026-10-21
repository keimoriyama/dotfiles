;;; marimo.el --- Edit and run marimo notebooks in Org -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1.0

;;; Commentary:
;; M-x marimo-open opens a native .py notebook as an Org document.  Python
;; blocks execute through marimo-lsp, not ob-python.  C-x C-s writes native
;; Python; results and runtime IDs are never written to the notebook.
;; See marimo.org for installation, protocol compatibility, and limitations.

;;; Code:

(require 'cl-lib)
(require 'jsonrpc)
(require 'org)
(require 'org-id)
(require 'ob-core)
(require 'dom)
(require 'shr)
(require 'url-util)

(defgroup marimo nil "Marimo notebooks in Org." :group 'org)
(require 'marimo-renderer)

(defcustom marimo-server-command
  '("uvx" "--from"
    "git+https://github.com/marimo-team/marimo-lsp.git@4fe0df21a01e123b57113cd65c27e703c7bf91ca"
    "marimo-lsp")
  "Command for the matching marimo-lsp server, without shell expansion."
  :type '(repeat string))

(defcustom marimo-python-command nil
  "Kernel Python executable, or nil to prefer the project's .venv/bin/python.
The selected environment must contain a compatible marimo installation."
  :type '(choice (const nil) file))

(defvar-local marimo--connection nil)
(defvar-local marimo--document nil)
(defvar-local marimo--version 0)
(defvar-local marimo--synced nil)
(defvar-local marimo--session nil)
(defvar-local marimo--retired-sessions nil)
(defvar-local marimo--outputs nil)
(defvar-local marimo--assets nil)
(defvar-local marimo--status nil)
(defvar-local marimo--sync-timer nil)
(defvar-local marimo--executing nil)
(defvar-local marimo--run-started nil)

(defun marimo--uri ()
  "Return the URI of this notebook."
  (unless (and buffer-file-name (not (file-remote-p buffer-file-name)))
    (user-error "Marimo requires a local notebook file"))
  (concat "file://" (mapconcat #'url-hexify-string
                               (split-string (expand-file-name buffer-file-name) "/") "/")))

(defun marimo--python ()
  "Resolve the kernel Python in the notebook's environment."
  (or (and marimo-python-command
           (or (executable-find marimo-python-command)
               (user-error "Kernel Python not found: %s" marimo-python-command)))
      (let ((root (locate-dominating-file default-directory ".venv")))
        (and root (let ((python (expand-file-name ".venv/bin/python" root)))
                    (and (file-executable-p python) python))))
      (executable-find "python3")
      (user-error "No Python executable found")))

(defun marimo--command (params)
  "Send protocol command PARAMS to this notebook's server."
  (unless (and marimo--connection (jsonrpc-running-p marimo--connection))
    (user-error "Marimo server disconnected; reopen the notebook"))
  (jsonrpc-request marimo--connection 'marimo/command params :timeout 60))

(defun marimo--connect ()
  "Start and initialize this notebook's dedicated LSP connection."
  (unless (and marimo-server-command
               (executable-find (car marimo-server-command)))
    (user-error "Server command unavailable: %S" marimo-server-command))
  (let* ((buffer (current-buffer))
         (name (format "marimo:%s" (buffer-name)))
         (command marimo-server-command))
    (setq marimo--connection
          (make-instance
           'jsonrpc-process-connection :name name
           :process (lambda ()
                      (make-process :name name :command command
                                    :connection-type 'pipe :noquery t
                                    :stderr (get-buffer-create
                                             (format "*%s stderr*" name))))
           :notification-dispatcher
           (lambda (_connection method params)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (condition-case err
                     (marimo--notification method params)
                   (error (message "Marimo notification: %s"
                                   (error-message-string err)))))))
           :on-shutdown
           (lambda (_connection)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq marimo--status "disconnected" marimo--executing nil)
                 (force-mode-line-update))))))
    (jsonrpc-request
     marimo--connection 'initialize
     (list :processId (emacs-pid) :rootUri (marimo--uri)
           :capabilities '(:notebookDocument (:synchronization (:dynamicRegistration :json-false))))
     :timeout 60)
    (jsonrpc-notify marimo--connection 'initialized (make-hash-table))
    ;; Fail before transforming the source if the installed server speaks a
    ;; different private protocol.
    (marimo--command '(:kind "list-sessions"))))

(defun marimo--cells ()
  "Read Python cells and validate unique runtime IDs in the Org document."
  (save-restriction
    (widen)
    (marimo--remove-cell-headings)
    (let ((seen (make-hash-table :test #'equal)))
      (org-element-map (org-element-parse-buffer) 'src-block
        (lambda (block)
          (when (equal (org-element-property :language block) "python")
            (save-excursion
              (goto-char (org-element-property :begin block))
              (let* ((attribute (car (org-element-property :attr_marimo block)))
                     (metadata (and attribute (json-parse-string attribute :object-type 'hash-table
                                                                  :false-object :json-false :null-object nil)))
                     (id (and (hash-table-p metadata) (gethash "id" metadata)))
                     (config (and id (gethash "config" metadata))))
                (unless (and id (not (gethash id seen)))
                  (user-error "Missing or duplicate marimo cell ID; use marimo-insert-cell"))
                (puthash id t seen)
                (list :id id :code (org-unescape-code-in-string
                                    (org-element-property :value block))
                      :name (or (gethash "name" metadata) "_")
                      :config (let ((value (or config (make-hash-table))))
                                (unless (hash-table-p value)
                                  (user-error "Marimo cell config must be a JSON object"))
                                value)
                      :begin (org-element-property :begin block))))))))))

(defun marimo--lsp-cells (cells)
  "Build LSP cells and text documents for CELLS."
  (let ((uri (marimo--uri)))
    (list
     :cells
     (vconcat
      (mapcar (lambda (cell)
                (list :kind 2 :document (concat uri "#" (plist-get cell :id))
                      :metadata
                      (list :marimo (list :name (plist-get cell :name)
                                          :options (plist-get cell :config))
                            :marimoRuntime (list :stableId (plist-get cell :id)))))
              cells))
     :texts
     (vconcat
      (mapcar (lambda (cell)
                (list :uri (concat uri "#" (plist-get cell :id))
                      :languageId "python" :version marimo--version
                      :text (plist-get cell :code)))
              cells)))))

(defun marimo--wire-document ()
  "Preserve JSON object types in the notebook document sent to the server."
  (let* ((document (copy-sequence marimo--document))
         (options (copy-tree (plist-get document :appOptions))))
    ;; jsonrpc decodes both {} and null as nil; these protocol fields are objects.
    (when (listp options)
      (setq options (plist-put options :passthrough
                               (or (plist-get options :passthrough) (make-hash-table)))))
    (setq document (plist-put document :appOptions options))
    (plist-put document :metadata (or (plist-get document :metadata) (make-hash-table)))))

(defun marimo--sync ()
  "Synchronize the Org cells, including insertions and deletions."
  (let* ((cells (marimo--cells))
         (previous (plist-get marimo--synced :cells))
         (signature (mapcar
                     (lambda (c) (list (plist-get c :id) (plist-get c :code)
                                       (plist-get c :name) (json-serialize (plist-get c :config)
                                                                           :false-object :json-false
                                                                           :null-object nil)))
                     cells))
         (uri (marimo--uri)))
    (when (or (null marimo--synced)
              (not (equal signature (plist-get marimo--synced :signature))))
      (cl-incf marimo--version)
      (let* ((wire (marimo--lsp-cells cells))
             (document (marimo--wire-document))
             (metadata (list :marimo
                             (list :appOptions (plist-get document :appOptions)
                                   :header (plist-get document :header)
                                   :notebookMetadata (plist-get document :metadata)))))
        (if marimo--synced
            (jsonrpc-notify
             marimo--connection 'notebookDocument/didChange
             (list :notebookDocument (list :uri uri :version marimo--version)
                   :change
                   (list :metadata metadata :cells
                         (list :structure
                               (list :array (list :start 0 :deleteCount (length previous)
                                                  :cells (plist-get wire :cells))
                                     :didClose (vconcat
                                                (mapcar (lambda (c) (list :uri (plist-get c :uri)))
                                                        (plist-get (marimo--lsp-cells previous) :texts)))
                                     :didOpen (plist-get wire :texts))))))
          (jsonrpc-notify
           marimo--connection 'notebookDocument/didOpen
           (list :notebookDocument
                 (list :uri uri :notebookType "marimo-notebook" :version marimo--version
                       :metadata metadata :cells (plist-get wire :cells))
                 :cellTextDocuments (plist-get wire :texts))))
        (setq marimo--synced (list :cells cells :signature signature)))
      (when marimo--session
        (dolist (old previous)
          (unless (cl-find (plist-get old :id) cells :key (lambda (c) (plist-get c :id)) :test #'equal)
            (marimo-renderer--dispose (plist-get old :id))
            (marimo--command (list :kind "delete-cell" :notebookUri uri
                                   :kernelSessionId marimo--session :cellId (plist-get old :id))))))))
  (marimo--cells))

(defun marimo--insert-metadata (cell)
  "Insert hidden affiliated Org metadata for CELL at point."
  (let ((begin (point)))
    (insert "#+attr_marimo: "
            (json-serialize (list :id (or (plist-get cell :id) (org-id-new))
                                  :name (or (plist-get cell :name) "_")
                                  :config (or (plist-get cell :config) (make-hash-table)))
                            :false-object :json-false :null-object nil)
            "\n")
    (add-to-invisibility-spec 'marimo-metadata)
    (let ((overlay (make-overlay begin (point) nil t nil)))
      (overlay-put overlay 'invisible 'marimo-metadata)
      (overlay-put overlay 'evaporate t))))

(defun marimo--remove-cell-headings ()
  "Migrate old heading-based cells without changing code or live results."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*:MARIMO_ID:" nil t)
      (let ((modified (buffer-modified-p)) (inhibit-modification-hooks t) cells)
        (org-element-map (org-element-parse-buffer) 'src-block
          (lambda (block)
            (when (and (equal (org-element-property :language block) "python")
                       (not (org-element-property :attr_marimo block)))
              (goto-char (org-element-property :begin block))
              (when-let* ((id (org-entry-get nil "MARIMO_ID")))
                (push (list :begin (point) :id id :name (org-entry-get nil "MARIMO_NAME")
                            :config (json-parse-string (or (org-entry-get nil "MARIMO_CONFIG") "{}")
                                                       :false-object :json-false :null-object nil)) cells)))))
        (dolist (cell cells)
          (goto-char (plist-get cell :begin))
          (let ((block (copy-marker (point))))
            (org-back-to-heading t)
            (let ((begin (point)))
              (org-end-of-meta-data t)
              (delete-region begin (point)))
            (goto-char block)
            (marimo--insert-metadata cell)))
        (goto-char (point-min))
        (when (and buffer-file-name
                   (looking-at (regexp-quote (concat "#+title: " (file-name-base buffer-file-name) "\n"))))
          (delete-region (point) (progn (forward-line 1) (point))))
        (org-element-cache-reset)
        (set-buffer-modified-p modified)))))

(defun marimo--insert (cell)
  "Insert a heading-free Org code block for serialized CELL."
  (marimo--insert-metadata cell)
  (insert "#+begin_src python\n"
          (org-escape-code-in-string (or (plist-get cell :code) "")))
  (unless (bolp) (insert "\n"))
  (insert "#+end_src\n\n"))

(defun marimo--initialize ()
  "Convert the current Python buffer into the Org notebook UI."
  (marimo--uri)
  (widen)
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (modified (buffer-modified-p)) success)
    (unwind-protect
        (progn
          (marimo--connect)
          (let ((parsed (marimo--command (list :kind "parse-notebook" :source source))))
            (unless (equal (plist-get parsed :kind) "success")
              (user-error "Not a valid marimo notebook: %S" parsed))
            (setq marimo--document (plist-get parsed :document)))
          (let ((inhibit-read-only t) (inhibit-modification-hooks t))
            (erase-buffer)
            (mapc #'marimo--insert (plist-get marimo--document :cells)))
          (setq buffer-undo-list nil)
          (set-buffer-modified-p modified)
          (goto-char (point-min))
          (org-fold-show-all)
          (org-fold-hide-drawer-all)
          (marimo--sync)
          (setq success t))
      (when (and (not success) marimo--connection)
        (jsonrpc-shutdown marimo--connection)))))

;;;###autoload
(defun marimo-open (file)
  "Open local marimo notebook FILE as an Org editing buffer."
  (interactive "fMarimo notebook: ")
  (when (file-remote-p file) (user-error "Remote notebooks are not supported"))
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (unless (and (derived-mode-p 'marimo-mode) marimo--document)
        (marimo-mode)))
    (pop-to-buffer buffer)))

(defun marimo-new (file)
  "Create a new marimo notebook at FILE without overwriting an existing file."
  (interactive "FNew marimo notebook: ")
  (when (or (file-exists-p file) (file-remote-p file) (not (string-suffix-p ".py" file)))
    (user-error "Choose a new local .py file"))
  (write-region "import marimo\napp = marimo.App()\n\n@app.cell\ndef _():\n    return\n\nif __name__ == \"__main__\":\n    app.run()\n"
                nil file nil 'silent nil 'excl)
  (marimo-open file))

(defun marimo-insert-cell ()
  "Append a new Python cell and move into its body."
  (interactive)
  (goto-char (point-max))
  (marimo--insert (list :id (org-id-new) :name "_" :code ""))
  (search-backward "#+end_src"))

(defun marimo--source ()
  "Generate Python from the current Org cells and retained metadata."
  (unless marimo--document (user-error "Open notebooks with marimo-open"))
  (let* ((document (marimo--wire-document))
         (cells (marimo--cells)))
    (setq document (plist-put document :cells
                              (vconcat (mapcar
                                        (lambda (cell)
                                          (list :id (plist-get cell :id)
                                                :code (plist-get cell :code)
                                                :code_hash nil :name (plist-get cell :name)
                                                :config (plist-get cell :config))) cells))))
    (let ((source (plist-get (marimo--command
                              (list :kind "print-notebook" :document document)) :source)))
      (unless (stringp source) (user-error "Server returned no Python source"))
      source)))

(defun marimo--save ()
  "Write the Org notebook as native Python; return non-nil for save hooks."
  (unless (verify-visited-file-modtime (current-buffer))
    (user-error "Notebook changed on disk; revert it before saving"))
  (write-region (marimo--source) nil buffer-file-name nil 'silent)
  (set-visited-file-modtime)
  (set-buffer-modified-p nil)
  t)

(defun marimo--revert (_ignore-auto _noconfirm)
  "Rebuild the Org projection from disk after normal revert confirmation."
  (widen)
  (let* ((file buffer-file-name)
         (source (with-temp-buffer (insert-file-contents file) (buffer-string)))
         (parsed (marimo--command (list :kind "parse-notebook" :source source))))
    (unless (equal (plist-get parsed :kind) "success")
      (user-error "Cannot revert: file is not a valid marimo notebook"))
    (marimo-stop)
    (let ((inhibit-modification-hooks t))
      (setq marimo--document (plist-get parsed :document))
      (clrhash marimo--outputs)
      (erase-buffer)
      (mapc #'marimo--insert (plist-get marimo--document :cells)))
    (setq buffer-undo-list nil)
    (set-visited-file-modtime)
    (set-buffer-modified-p nil)
    (marimo--sync)))

(defun marimo--execute (cells)
  "Execute CELLS asynchronously in the persistent notebook kernel."
  (unless cells (user-error "No Python cells to execute"))
  (when marimo--executing (user-error "Marimo is executing; interrupt or wait before running again"))
  (let ((buffer (current-buffer)) (python (marimo--python)))
    (dolist (cell (marimo--cells))
      (let* ((id (plist-get cell :id)) (state (gethash id marimo--outputs)))
        (puthash id (plist-put state :submitted (plist-get cell :code)) marimo--outputs)))
    (setq marimo--status "running" marimo--executing t marimo--run-started nil)
    (force-mode-line-update)
    (jsonrpc-async-request
     marimo--connection 'marimo/command
     (list :kind "execute" :notebookUri (marimo--uri)
           :executable python :workingDirectory default-directory
           :cells (vconcat (mapcar (lambda (c) (list :cellId (plist-get c :id)
                                                     :code (plist-get c :code))) cells)))
     :timeout 60
     :success-fn (lambda (snapshot)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (marimo--sessions snapshot))))
     :timeout-fn (lambda ()
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (setq marimo--executing nil marimo--status "request timed out")
                       (force-mode-line-update))))
     :error-fn (lambda (err)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq marimo--status "failed" marimo--executing nil)
                     (force-mode-line-update)
                     (message "Marimo execution failed: %S" err)))))))

(defun marimo-run-cell ()
  "Synchronize the notebook and execute the Python cell at point."
  (interactive)
  (let* ((context (org-element-context))
         (block (if (eq (org-element-type context) 'src-block) context
                  (org-element-lineage context '(src-block) t)))
         (begin (and block (org-element-property :begin block))))
    (unless begin (user-error "Point must be inside a Python source block"))
    (let ((cell (cl-find begin (marimo--sync) :key (lambda (c) (plist-get c :begin)))))
      (unless cell (user-error "Point must be inside a Python source block"))
      (marimo--execute (list cell)))))

(defun marimo-run-all ()
  "Execute every cell in the same kernel."
  (interactive)
  (marimo--execute (marimo--sync)))

(defun marimo-run-stale ()
  "Execute cells whose code or dependencies are stale."
  (interactive)
  (marimo--execute
   (cl-remove-if-not
    (lambda (cell)
      (let ((state (gethash (plist-get cell :id) marimo--outputs)))
        (or (plist-get state :stale)
            (not (equal (plist-get state :executed) (plist-get cell :code))))))
    (marimo--sync))))

(defun marimo-interrupt ()
  "Interrupt execution without discarding the kernel."
  (interactive)
  (unless marimo--session (user-error "No running marimo kernel"))
  (marimo--command (list :kind "interrupt" :notebookUri (marimo--uri)
                         :kernelSessionId marimo--session)))

(defun marimo-restart-kernel ()
  "Explicitly discard kernel state and start a fresh session."
  (interactive)
  (marimo--sync)
  (marimo-renderer--reset)
  (when marimo--session (push marimo--session marimo--retired-sessions))
  (setq marimo--session nil marimo--executing nil)
  (marimo--command (list :kind "restart-session" :notebookUri (marimo--uri)
                         :executable (marimo--python) :workingDirectory default-directory
                         :createIfMissing t))
  (clrhash marimo--outputs)
  (marimo--sessions (marimo--command '(:kind "list-sessions"))))

(defun marimo-stop ()
  "Stop this notebook's kernel; retain the connection for future execution."
  (interactive)
  (marimo-renderer--reset)
  (marimo--command (list :kind "close-session" :notebookUri (marimo--uri)))
  (when marimo--session (push marimo--session marimo--retired-sessions))
  (setq marimo--session nil marimo--status "stopped" marimo--executing nil)
  (force-mode-line-update))

(defun marimo-show-log ()
  "Display this notebook's JSON-RPC log."
  (interactive)
  (unless marimo--connection (user-error "No marimo connection"))
  (pop-to-buffer (jsonrpc-events-buffer marimo--connection)))

(defun marimo--sessions (snapshot)
  "Update kernel identity and status from SNAPSHOT."
  (mapc (lambda (session)
          (when (and (equal (plist-get session :notebookUri) (marimo--uri))
                     (not (member (plist-get session :sessionId) marimo--retired-sessions)))
            (setq marimo--session (plist-get session :sessionId)
                  marimo--status (plist-get session :status))
            (force-mode-line-update)))
        (plist-get snapshot :sessions)))

(defun marimo--notification (method params)
  "Handle server METHOD with PARAMS in the owning notebook buffer."
  (pcase method
    ('marimo/sessionsChanged (marimo--sessions params))
    ('marimo/kernelNotification
     (let ((session (plist-get params :sessionId)))
       (when (and (equal (plist-get params :notebookUri) (marimo--uri))
                  (not (member session marimo--retired-sessions))
                  (or (null marimo--session) (equal session marimo--session)))
         (setq marimo--session session)
         (let ((op (plist-get params :notification)))
           (marimo-renderer-notify op)
           (pcase (plist-get op :op)
             ("cell-op"
              (when (equal (plist-get op :status) "running")
                (setq marimo--run-started t))
              (marimo--cell-output op))
             ("completed-run"
              (when marimo--run-started (setq marimo--executing nil)))
             ("interrupted" (setq marimo--executing nil)))))))))

(defun marimo--cell-output (op)
  "Merge cell operation OP, preserving omitted output fields."
  (let* ((id (plist-get op :cell_id)) (state (gethash id marimo--outputs))
         (status (plist-get op :status)) (console (plist-get op :console)))
    (when status
      (setq state (plist-put state :status status))
      (when (equal status "running")
        (setq state (plist-put state :output nil)
              state (plist-put state :console nil)
              state (plist-put state :executed (plist-get state :submitted)))))
    (when (plist-get op :output)
      (setq state (plist-put state :output (plist-get op :output))))
    (when console
      (setq state (plist-put state :console
                             (if (vectorp console) (append console nil)
                               (append (plist-get state :console) (list console))))))
    (when (memq (plist-get op :stale_inputs) '(t :json-false))
      (setq state (plist-put state :stale (eq (plist-get op :stale_inputs) t))))
    (puthash id state marimo--outputs)
    (marimo--render id state)))

(defun marimo--image (mime data)
  "Save base64 DATA with image MIME in this buffer's temporary asset directory."
  (unless (and (member mime '("image/png" "image/jpeg" "image/svg+xml"))
               (stringp data))
    (error "Unsupported image output: %s" mime))
  (unless marimo--assets (setq marimo--assets (make-temp-file "marimo-org-" t)))
  (let* ((bytes (cond
                 ((string-prefix-p "data:" data)
                  (let ((comma (string-search "," data)))
                    (unless comma (error "Malformed image data URL"))
                    (let ((header (substring data 0 comma))
                          (body (url-unhex-string (substring data (1+ comma)))))
                      (if (string-suffix-p ";base64" header) (base64-decode-string body) body))))
                 ((and (equal mime "image/svg+xml") (string-prefix-p "<" data))
                  (encode-coding-string data 'utf-8))
                 (t (base64-decode-string data))))
         (file (expand-file-name
                (concat (secure-hash 'sha256 bytes)
                        (pcase mime ("image/png" ".png") ("image/jpeg" ".jpg") (_ ".svg")))
                marimo--assets)))
    (unless (file-exists-p file)
      (let ((coding-system-for-write 'no-conversion)) (write-region bytes nil file nil 'silent)))
    (format "[[file:%s]]\n" file)))

(defun marimo--format-output (output)
  "Convert OUTPUT into safe Org text or an inline image link."
  (let ((data (plist-get output :data)) (mime (plist-get output :mimetype)))
    (cond
     ((null data) "")
     ((member mime '("image/png" "image/jpeg" "image/svg+xml"))
      (marimo--image mime data))
     ((equal mime "application/vnd.marimo+error")
      (marimo--example
       (mapconcat (lambda (entry)
                    (format "%s: %s" (or (plist-get entry :exception_type) (plist-get entry :type) "Error")
                            (or (plist-get entry :msg) entry)))
                  data "\n")))
     ((and (or (member mime '("text/html" "application/vnd.marimo+traceback"))
               (and (equal mime "text/markdown") (stringp data) (string-match-p "<[a-zA-Z]" data)))
           (stringp data) (fboundp 'libxml-parse-html-region))
      (let* ((dom (with-temp-buffer (insert data) (libxml-parse-html-region (point-min) (point-max))))
             (images (dom-by-tag dom 'img))
             (tables (dom-by-tag dom 'table))
             (shr-inhibit-images t) (shr-use-fonts nil)
             (text (with-temp-buffer (shr-insert-document dom) (buffer-string))))
        (concat
         (if tables
             (mapconcat
              (lambda (table)
                (mapconcat
                 (lambda (row)
                   (concat "| "
                           (mapconcat (lambda (col)
                                        (replace-regexp-in-string "[|\n\r]" " " (dom-texts col)))
                                      (cl-remove-if-not
                                       (lambda (n) (and (listp n) (memq (dom-tag n) '(td th))))
                                       (dom-children row)) " | ") " |\n"))
                 (dom-by-tag table 'tr) "")) tables "\n")
           (marimo--example text))
         (mapconcat
          (lambda (img)
            (let ((src (dom-attr img 'src)))
              (if (and src (string-match "\\`data:\\(image/[^;]+\\);base64,\\(.*\\)\\'" src))
                  (marimo--image (match-string 1 src) (match-string 2 src))
                "[Image requires an external resource]\n"))) images ""))))
     (t (marimo--example (if (stringp data) data (format "%S" data)))))))

(defun marimo--example (text)
  "Quote TEXT so output cannot create executable Org blocks."
  (if (string-empty-p (string-trim text)) ""
    (let ((case-fold-search t))
      (concat "#+begin_example\n"
              (replace-regexp-in-string "^\\([ \t]*:end:[ \t]*\\)$" ",\\1"
                                        (org-escape-code-in-string text))
              (unless (string-suffix-p "\n" text) "\n") "#+end_example\n"))))

(defun marimo--render (id state)
  "Render cell ID's STATE using native Org or the official rich renderer."
  (save-restriction
    (widen)
    (if (or (marimo-renderer-needed-p state)
            (and marimo-renderer--widgets (gethash id marimo-renderer--widgets)
                 (null (plist-get state :output))))
        (condition-case err
            (marimo-renderer-render id state)
          (error
           (let ((fallback (copy-sequence state)))
             (setq fallback (plist-put fallback :console
                                        (cons (list :mimetype "text/plain"
                                                    :data (format "Rich output: %s" (error-message-string err)))
                                              (plist-get state :console))))
             (marimo--render-native id fallback))))
      (marimo-renderer--dispose id)
      (marimo--render-native id state))))

(defun marimo--render-native (id state)
  "Replace the results of cell ID from STATE without changing source state."
  (save-restriction
    (widen)
    (let ((cell (cl-find id (marimo--cells) :key (lambda (c) (plist-get c :id)) :test #'equal)))
      (when cell
        (let* ((modified (buffer-modified-p))
               (inhibit-modification-hooks t)
               (text (concat (mapconcat #'marimo--format-output (plist-get state :console) "")
                             (marimo--format-output (plist-get state :output))))
               (stale (or (plist-get state :stale)
                          (not (equal (plist-get state :executed) (plist-get cell :code))))))
          ;; Result text shifts the following source.  Keep its undo records so
          ;; older source edits retain valid positions when undone.
          (undo-boundary)
          (save-excursion
            (goto-char (plist-get cell :begin))
            (org-babel-remove-result)
            (goto-char (org-element-property :end (org-element-at-point)))
            (insert "#+RESULTS:\n:results:\n"
                    (format ": %s%s\n" (or (plist-get state :status) "idle")
                            (if stale " (stale)" "")) text ":end:\n\n"))
          (org-element-cache-reset)
          (set-buffer-modified-p modified)
          (undo-boundary)
          (when (display-graphic-p) (org-display-inline-images)))))))

(defun marimo--cleanup ()
  "Close the server and its kernel when this editing buffer is killed."
  (when (timerp marimo--sync-timer) (cancel-timer marimo--sync-timer))
  (marimo-renderer--cleanup)
  (when (and marimo--connection (jsonrpc-running-p marimo--connection))
    (ignore-errors (jsonrpc-request marimo--connection 'shutdown nil :timeout 3))
    (ignore-errors (jsonrpc-notify marimo--connection 'exit nil))
    (jsonrpc-shutdown marimo--connection)))

(defun marimo--changed (&rest _)
  "Schedule source synchronization after editing has paused."
  (when (timerp marimo--sync-timer) (cancel-timer marimo--sync-timer))
  (let ((buffer (current-buffer)))
    (setq marimo--sync-timer
          (run-with-idle-timer
           0.5 nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (and marimo--connection (jsonrpc-running-p marimo--connection))
                   (condition-case err (marimo--sync)
                     (error (message "Marimo sync: %s" (error-message-string err))))))))))))

(defun marimo--leave ()
  "Restore Python source before changing major mode."
  (widen)
  (when marimo--document
    (let ((modified (buffer-modified-p)))
      (let ((source (marimo--source))
            (inhibit-modification-hooks t))
        (erase-buffer)
        (insert source)
        (set-buffer-modified-p modified))))
  (marimo--cleanup))

(defvar marimo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'marimo-run-cell)
    (define-key map (kbd "C-c C-a") #'marimo-insert-cell)
    (define-key map (kbd "C-c C-k") #'marimo-interrupt)
    (define-key map (kbd "C-c C-r") #'marimo-run-stale)
    map))

;;;###autoload
(define-derived-mode marimo-mode org-mode "Marimo"
  "Edit native marimo notebooks through Org source blocks.
Activating this mode in a file buffer converts its Python source to Org.
\\<marimo-mode-map>\\[marimo-run-cell] executes a cell; \\[save-buffer] saves native Python."
  (setq-local marimo--outputs (make-hash-table :test #'equal))
  (setq-local marimo--status "ready")
  (setq-local mode-line-process '(:eval (format " [%s]" marimo--status)))
  ;; Autosaving this projection to the visited .py would destroy the notebook.
  (auto-save-mode -1)
  (setq-local buffer-auto-save-file-name nil)
  (setq-local revert-buffer-function #'marimo--revert)
  (add-hook 'write-contents-functions #'marimo--save nil t)
  (add-hook 'after-change-functions #'marimo--changed nil t)
  (add-hook 'kill-buffer-hook #'marimo--cleanup nil t)
  (add-hook 'change-major-mode-hook #'marimo--leave nil t)
  (when buffer-file-name (marimo--initialize)))

(provide 'marimo)
;;; marimo.el ends here
