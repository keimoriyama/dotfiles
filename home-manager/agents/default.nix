{
  config,
  lib,
  isWork ? false,
  ...
}: let
  # Claude Code と Codex で同じ指示文書を読ませるため、実体は 1 つにして
  # それぞれが探すファイル名で symlink を張る。
  agentsDoc = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home-manager/agents/AGENTS.md";
in {
  home.file.".claude/CLAUDE.md" = {
    source = agentsDoc;
    force = true;
  };

  # codex 本体を入れない業務用マシンでは AGENTS.md も配置しない。
  home.file.".codex/AGENTS.md" = lib.mkIf (!isWork) {
    source = agentsDoc;
    force = true;
  };
}
