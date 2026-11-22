;;; marimo-renderer.el --- Official marimo output renderer in Org -*- lexical-binding: t; -*-

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'url)
(require 'org)
(require 'emacs-egui)

(defvar marimo--outputs)
(defvar marimo--session)
(defvar marimo--connection)
(declare-function marimo--uri "marimo")
(declare-function marimo--cells "marimo")
(declare-function marimo--sync "marimo")
(declare-function jsonrpc-async-request "jsonrpc")
(declare-function xwidget-live-p "xwidget.c")
(declare-function xwidget-buffer "xwidget.c")
(declare-function xwidget-resize "xwidget.c")
(declare-function xwidget-info "xwidget.c")
(declare-function set-xwidget-buffer "xwidget.c")
(declare-function set-xwidget-query-on-exit-flag "xwidget.c")

(defconst marimo-renderer--directory (file-name-directory (or load-file-name buffer-file-name)))
(defconst marimo-renderer--app-name "marimo-renderer-0-18-3")
(defconst marimo-renderer--archive
  "https://open-vsx.org/api/marimo-team/vscode-marimo/0.18.3/file/marimo-team.vscode-marimo-0.18.3.vsix")
(defconst marimo-renderer--sha256 "e36ddf808c7ef60ecc28869f4d96b1904ee1d037488ae655c25098bfb3c1d661")

(defcustom marimo-renderer-assets-directory
  (let ((bundled (expand-file-name "marimo-renderer-vendor" marimo-renderer--directory)))
    (if (file-exists-p (expand-file-name "renderer.mjs" bundled)) bundled
      (locate-user-emacs-file "var/marimo-renderer/0.18.3/extension/dist/")))
  "Directory containing the official VS Code 0.18.3 renderer and its chunks."
  :group 'marimo :type 'directory)

(defcustom marimo-renderer-ui-directory
  (locate-user-emacs-file "var/marimo-egui/0.18.3/")
  "Directory used to serve the renderer through emacs-egui."
  :group 'marimo :type 'directory)

(defvar-local marimo-renderer--widgets nil)
(defvar-local marimo-renderer--error nil)
(defvar-local marimo-renderer--laying-out nil)

;;;###autoload
(defun marimo-install-renderer ()
  "Download and verify the pinned official renderer for WebKit output."
  (interactive)
  (unless (executable-find "unzip") (user-error "Installing the renderer requires unzip"))
  (let ((archive (make-temp-file "marimo-renderer-" nil ".vsix"))
        (directory (make-temp-file "marimo-renderer-" t)))
    (unwind-protect
        (progn
          (url-copy-file marimo-renderer--archive archive t)
          (unless (equal (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally archive)
                           (secure-hash 'sha256 (current-buffer)))
                         marimo-renderer--sha256)
            (user-error "Renderer archive checksum mismatch"))
          (unless (zerop (call-process "unzip" nil nil nil "-q" archive
                                      "extension/dist/*" "extension/LICENSE.txt" "-d" directory))
            (user-error "Could not extract renderer assets"))
          (make-directory marimo-renderer-assets-directory t)
          (copy-directory (expand-file-name "extension/dist/" directory)
                          marimo-renderer-assets-directory nil t t)
          (copy-file (expand-file-name "extension/LICENSE.txt" directory)
                     (expand-file-name "LICENSE.txt" marimo-renderer-assets-directory) t)
          (message "Installed marimo renderer 0.18.3"))
      (delete-file archive)
      (delete-directory directory t))))

(defun marimo-renderer-needed-p (state)
  "Return non-nil when STATE needs the official JavaScript renderer."
  (let* ((output (plist-get state :output))
         (mime (plist-get output :mimetype))
         (data (plist-get output :data)))
    (and output
         (or (and (stringp data) (string-match-p "<\\(?:marimo-\\|iframe\\|script\\|svg\\)" data))
             (and mime (not (member mime '("text/plain" "text/html" "text/markdown"
                                           "image/png" "image/jpeg" "image/svg+xml"
                                           "application/vnd.marimo+error"
                                           "application/vnd.marimo+traceback"))))))))

(defun marimo-renderer--prepare-app ()
  "Register the official renderer as an emacs-egui application."
  (make-directory marimo-renderer-ui-directory t)
  (copy-file (expand-file-name "marimo-renderer.html" marimo-renderer--directory)
             (expand-file-name "index.html" marimo-renderer-ui-directory) t)
  (let ((vendor (expand-file-name "vendor" marimo-renderer-ui-directory))
        (assets (file-truename marimo-renderer-assets-directory)))
    (when (and (file-symlink-p vendor)
               (not (equal (file-truename vendor) assets)))
      (delete-file vendor))
    (unless (file-exists-p vendor)
      (make-symbolic-link assets vendor)))
  (emacs-egui-register-app marimo-renderer--app-name marimo-renderer-ui-directory)
  (emacs-egui-ensure-server))

(defun marimo-renderer--start ()
  "Prepare the emacs-egui host for this notebook's rich output."
  (unless (and (display-graphic-p) (featurep 'xwidget-internal))
    (user-error "Rich output requires an Emacs build with WebKit"))
  (require 'xwidget)
  (unless (file-exists-p (expand-file-name "renderer.mjs" marimo-renderer-assets-directory))
    (user-error "Run M-x marimo-install-renderer to install rich output support"))
  (marimo-renderer--prepare-app))

(defun marimo-renderer--json-plist (value)
  "Convert JSON alists in VALUE to plists, preserving vectors."
  (cond
   ((vectorp value)
    (vconcat (mapcar #'marimo-renderer--json-plist value)))
   ((and (listp value) (cl-every #'consp value))
    (cl-loop for (key . item) in value
             append (list (intern (concat ":" (symbol-name key)))
                          (marimo-renderer--json-plist item))))
   (t value)))

(defun marimo-renderer--flush (record)
  "Send messages queued for RECORD until its browser became ready."
  (when (plist-get record :ready)
    (let ((messages (nreverse (plist-get record :pending))))
      (plist-put record :pending nil)
      (dolist (message messages)
        (emacs-egui-send-state (plist-get record :session) message)))))

(defun marimo-renderer--send-record (record message)
  "Send MESSAGE to RECORD's browser, or queue it until initialization ends."
  (if (plist-get record :ready)
      (emacs-egui-send-state (plist-get record :session) message)
    (plist-put record :pending (cons message (plist-get record :pending)))))

(defun marimo-renderer--send (message)
  "Route MESSAGE to one cell or broadcast a kernel notification."
  (let ((id (plist-get message :cellId)))
    (if id
        (when-let* ((record (and marimo-renderer--widgets
                                 (gethash id marimo-renderer--widgets))))
          (marimo-renderer--send-record record message))
      (when marimo-renderer--widgets
        (maphash (lambda (_id record)
                   (marimo-renderer--send-record record message))
                 marimo-renderer--widgets)))))

(defun marimo-renderer--request (event)
  "Dispatch a renderer EVENT to the owning kernel."
  (let* ((id (plist-get event :cellId))
         (record (and marimo-renderer--widgets (gethash id marimo-renderer--widgets)))
         (params (plist-get event :params))
         (request-id (plist-get event :requestId))
         (reply (lambda (&optional error)
                  (when record
                    (marimo-renderer--send-record
                     record (list :type "reply" :requestId request-id :error error))))))
    (if (not (and record (equal (plist-get event :sessionId) marimo--session)))
        (funcall reply "Stale renderer session")
      (pcase (plist-get event :command)
        ("resize"
         (let ((height (plist-get params :height)))
           (when (numberp height)
             (setq height (max 48 (min 1200 (ceiling height))))
             (plist-put record :height height)
             (when (xwidget-live-p (plist-get record :widget))
               (if-let* ((window (get-buffer-window)))
                   (marimo-renderer--layout window)
                 (xwidget-resize (plist-get record :widget) (plist-get record :width) height)))))
         (funcall reply))
        ("renderer-error"
         (setq marimo-renderer--error (plist-get params :message))
         (message "Marimo renderer: %s" marimo-renderer--error)
         (funcall reply))
        ("navigate-to-cell"
         (let ((cell (cl-find (plist-get params :cellId) (marimo--cells)
                              :key (lambda (c) (plist-get c :id)) :test #'equal)))
           (when cell (goto-char (plist-get cell :begin))))
         (funcall reply))
        ((or "update-ui-element" "invoke-function" "set-model-value")
         (let ((command (copy-sequence params)))
           ;; The browser cannot choose another notebook or kernel session.
           (setq command (plist-put command :kind (plist-get event :command))
                 command (plist-put command :notebookUri (marimo--uri))
                 command (plist-put command :kernelSessionId marimo--session))
           (when (equal (plist-get event :command) "invoke-function")
             (setq command (plist-put command :args (or (plist-get command :args) (make-hash-table)))))
           (jsonrpc-async-request marimo--connection 'marimo/command command
                                  :success-fn (lambda (_) (funcall reply))
                                  :error-fn (lambda (err) (funcall reply (format "%S" err)))
                                  :timeout-fn (lambda () (funcall reply "Kernel request timed out")))))
        (_ (funcall reply "Image toolbar is not supported; use the Org image link"))))))

(defun marimo-renderer--new-session (id)
  "Create an emacs-egui session for cell ID and return its display record."
  (let* ((notebook (current-buffer))
         (session (emacs-egui-create-buffer
                   :app-name marimo-renderer--app-name
                   :buffer-name (format " *marimo egui:%s*" id)))
         (record (list :session session :widget (plist-get session :xwidget)
                       :height 320 :ready nil :pending nil)))
    (emacs-egui-on
     session "ready"
     (lambda (_payload)
       (when (buffer-live-p notebook)
         (with-current-buffer notebook
           (plist-put record :ready t)
           (marimo-renderer--flush record)))))
    (emacs-egui-on
     session "request"
     (lambda (payload)
       (when (buffer-live-p notebook)
         (with-current-buffer notebook
           (marimo-renderer--request (marimo-renderer--json-plist payload))))))
    record))

(defun marimo-renderer--dispose (id)
  "Release cell ID's emacs-egui session, webview, and display state."
  (when-let* ((record (and marimo-renderer--widgets (gethash id marimo-renderer--widgets))))
    (marimo-renderer--send-record record (list :type "remove" :cellId id))
    (when (overlayp (plist-get record :overlay))
      (delete-overlay (plist-get record :overlay)))
    (let ((widget (plist-get record :widget))
          (owner (plist-get (plist-get record :session) :buffer)))
      (set-xwidget-query-on-exit-flag widget nil)
      (if (buffer-live-p owner)
          (kill-buffer owner)
        (when (and widget (fboundp 'xwidget-live-p) (xwidget-live-p widget))
          ;; Killing the owner buffer is Emacs's supported widget destructor.
          (let ((owner (generate-new-buffer " *marimo disposed widget*")))
            (set-xwidget-buffer widget owner)
            (kill-buffer owner)))))
    (remhash id marimo-renderer--widgets)))

(defun marimo-renderer--reset ()
  "Dispose rich output when its kernel session ends."
  (when marimo-renderer--widgets
    (mapc #'marimo-renderer--dispose (hash-table-keys marimo-renderer--widgets))))

(defun marimo-renderer--cleanup ()
  "Dispose all emacs-egui sessions owned by this notebook."
  (marimo-renderer--reset))

(defun marimo-renderer-notify (notification)
  "Relay a widget NOTIFICATION from the kernel to every renderer view."
  (when (member (plist-get notification :op)
                '("send-ui-element-message" "function-call-result" "model-lifecycle" "remove-ui-elements"))
    (marimo-renderer--send (list :type "notification" :sessionId marimo--session
                                 :notification notification))))

(defun marimo-renderer--layout (window)
  "Keep webviews inside WINDOW, including when the minibuffer grows."
  (when (and (not marimo-renderer--laying-out) marimo-renderer--widgets
             (window-live-p window) (eq (window-buffer window) (current-buffer)))
    (let ((marimo-renderer--laying-out t))
      (maphash
       (lambda (_id record)
         (when-let* ((position (pos-visible-in-window-p (plist-get record :marker) window t)))
           (let* ((widget (plist-get record :widget))
                  (width (max 1 (- (window-body-width window t) 32)))
                  ;; Cocoa does not reliably clip a WKWebView's subviews at
                  ;; the window edge.  Constrain the actual viewport instead.
                  (height (min (plist-get record :height)
                               (max 0 (- (window-body-height window t) (cadr position) 2))))
                  (info (xwidget-info widget)))
             (plist-put record :width width)
             (unless (and (= width (aref info 2)) (= height (aref info 3)))
               (xwidget-resize widget width height)))))
       marimo-renderer--widgets))))

(defun marimo-renderer-render (id state)
  "Display official renderer STATE inline at Org cell ID's result position."
  (marimo-renderer--start)
  (add-hook 'pre-redisplay-functions #'marimo-renderer--layout nil t)
  (setq state (or (gethash id marimo--outputs) state))
  (unless marimo-renderer--widgets
    (setq marimo-renderer--widgets (make-hash-table :test #'equal)))
  (let ((record (gethash id marimo-renderer--widgets)))
    (unless (and record (xwidget-live-p (plist-get record :widget))
                 (overlayp (plist-get record :overlay))
                 (overlay-buffer (plist-get record :overlay))
                 (marker-buffer (plist-get record :marker))
                 (equal (get-text-property (plist-get record :marker) 'marimo-cell) id))
      (marimo-renderer--dispose id)
      (let ((cell (cl-find id (marimo--cells) :key (lambda (c) (plist-get c :id)) :test #'equal))
            (modified (buffer-modified-p)) (inhibit-modification-hooks t))
        (when cell
          (undo-boundary)
          (save-excursion
            (goto-char (plist-get cell :begin))
            (org-babel-remove-result)
            (goto-char (org-element-property :end (org-element-at-point)))
            (insert "#+RESULTS:\n:results:\n")
            (let ((marker (copy-marker (point)))
                  (width (max 200 (- (window-body-width (or (get-buffer-window) (selected-window)) t) 32))))
              (insert " \n:end:\n\n")
              (let* ((record (marimo-renderer--new-session id))
                     (widget (plist-get record :widget))
                     (overlay (make-overlay marker (1+ marker))))
                ;; Org and prettify-symbols own the display text property.
                ;; An overlay survives refontification and remains clickable.
                (overlay-put overlay 'display (list 'xwidget :xwidget widget))
                (overlay-put overlay 'evaporate t)
                (set-xwidget-query-on-exit-flag widget nil)
                (xwidget-resize widget width 320)
                (put-text-property marker (1+ marker) 'marimo-cell id)
                (setf (plist-get record :overlay) overlay
                      (plist-get record :marker) marker
                      (plist-get record :width) width)
                (puthash id record marimo-renderer--widgets)))
          (org-element-cache-reset)
          (set-buffer-modified-p modified)
          (undo-boundary))))
    (marimo-renderer--send
     (list :type "state" :cellId id :sessionId marimo--session
           :state (list :output (plist-get state :output)
                        :consoleOutputs (vconcat (plist-get state :console))
                        :status (or (plist-get state :status) "idle")
                        :staleInputs (if (plist-get state :stale) t :json-false)))))))

(provide 'marimo-renderer)
;;; marimo-renderer.el ends here
