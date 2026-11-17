{
  config,
  lib,
  pkgs,
  ...
}: let
  updateCodexConfig = pkgs.writers.writePython3 "update-codex-config" {
    libraries = [pkgs.python3Packages.tomlkit];
  } (builtins.readFile ./update-codex-config.py);
in {
  home.file.".claude/CLAUDE.md" = {
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/ghq/github.com/keimoriyama/dotfiles/home-manager/agents/AGENTS.md";
    force = true;
  };

  home.file.".codex/AGENTS.md" = {
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/ghq/github.com/keimoriyama/dotfiles/home-manager/agents/AGENTS.md";
    force = true;
  };

  # Codex/ChatGPT も更新する可変ファイルなので、全体を管理せず必要な値だけをマージする。
  home.activation.codexAgentReviewWritableRoot = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD ${updateCodexConfig} \
      "${config.home.homeDirectory}/.codex/config.toml" \
      "${config.home.homeDirectory}/.local/agent-review"
  '';
}
