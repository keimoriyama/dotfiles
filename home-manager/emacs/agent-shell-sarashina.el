;;; agent-shell-sarashina.el --- Chat with Sarashina through OpenCode -*- lexical-binding: t; -*-

;;; Commentary:

;; Adds a "Sarashina" agent to agent-shell.  agent-shell only speaks ACP, so
;; the requests go through `opencode acp', started with a dedicated config
;; file that registers the Sarashina API as an OpenAI-compatible provider and
;; leaves a single tool-less "chat" agent.  The project's and the global
;; OpenCode/Claude Code setup are kept out so the session stays a plain chat.

(require 'agent-shell-opencode)
(require 'map)
(require 'subr-x)

(defcustom my-agent-shell-sarashina-opencode-config
  (expand-file-name "~/.config/opencode/sarashina.json")
  "OpenCode config file that defines the Sarashina provider and chat agent."
  :type 'file)

(defcustom my-agent-shell-sarashina-default-model-id "sarashina/sarashina3.2-mini-thinking"
  "OpenCode model ID selected when a Sarashina shell starts."
  :type 'string)

(defconst my-agent-shell-sarashina--isolation-environment
  '("OPENCODE_DISABLE_PROJECT_CONFIG=1"
    "OPENCODE_DISABLE_CLAUDE_CODE=1"
    "OPENCODE_DISABLE_EXTERNAL_SKILLS=1"
    "OPENCODE_DISABLE_DEFAULT_PLUGINS=1"
    "OPENCODE_DISABLE_AUTOUPDATE=1"
    "OPENCODE_DISABLE_LSP_DOWNLOAD=1")
  "Environment that keeps project and Claude Code settings out of OpenCode.")

(defun my-agent-shell-sarashina--api-key ()
  "Return the Sarashina API key from the SARASHINA_API_KEY environment variable."
  (let ((key (getenv "SARASHINA_API_KEY")))
    (if (and key (not (string-empty-p key)))
        key
      (user-error "環境変数 SARASHINA_API_KEY が設定されていません (~/.config/zsh/secrets.zsh で export し、Emacs を再起動してください)"))))

(defun my-agent-shell-sarashina-environment (api-key)
  "Return the OpenCode process environment for a Sarashina chat with API-KEY."
  (append (list (concat "OPENCODE_CONFIG=" my-agent-shell-sarashina-opencode-config)
                (concat "SARASHINA_API_KEY=" api-key))
          my-agent-shell-sarashina--isolation-environment))

(defun my-agent-shell-sarashina-make-client (buffer)
  "Create an OpenCode ACP client for a Sarashina chat in BUFFER."
  (let ((agent-shell-opencode-authentication
         (agent-shell-opencode-make-authentication :none t))
        (agent-shell-opencode-environment
         (append (my-agent-shell-sarashina-environment
                  (my-agent-shell-sarashina--api-key))
                 agent-shell-opencode-environment)))
    (agent-shell-opencode-make-client :buffer buffer)))

(defun my-agent-shell-sarashina-make-agent-config ()
  "Create the agent-shell configuration for chatting with Sarashina."
  (agent-shell-make-agent-config
   :identifier 'sarashina
   :mode-line-name "Sarashina"
   :buffer-name "Sarashina"
   :shell-prompt "Sarashina> "
   :shell-prompt-regexp "Sarashina> "
   :welcome-function #'agent-shell-opencode--welcome-message
   :icon-name "opencode.png"
   :client-maker #'my-agent-shell-sarashina-make-client
   :default-model-id (lambda () my-agent-shell-sarashina-default-model-id)
   :default-session-mode-id (lambda () "chat")
   :install-instructions "See https://opencode.ai/docs for installation."))

(defun my-agent-shell-sarashina-start ()
  "Start a new agent-shell chat with Sarashina."
  (interactive)
  (agent-shell--dwim :config (my-agent-shell-sarashina-make-agent-config)
                     :new-shell t))

(provide 'agent-shell-sarashina)
;;; agent-shell-sarashina.el ends here
