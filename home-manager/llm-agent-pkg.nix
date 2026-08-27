{
  llmAgentsPkgs,
  isWork ? false,
}:
with llmAgentsPkgs;
  [
    # ai
    # copilot-cli
    # codex
    copilot-language-server
    claude-code
    claude-agent-acp
    opencode
  ]
  # 業務用マシンでは codex 系を入れない。
  ++ (
    if isWork
    then []
    else [codex-acp]
  )
