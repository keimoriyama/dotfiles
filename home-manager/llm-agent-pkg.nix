{
  llmAgentsPkgs,
  isWork ? false,
}:
with llmAgentsPkgs;
  [
    # ai
    # copilot-cli
    copilot-language-server
    claude-code
    claude-agent-acp
    opencode
    herdr
  ]
  # 業務用マシンでは codex 系を入れない。
  ++ (
    if isWork
    then []
    else [codex-acp codex]
  )
