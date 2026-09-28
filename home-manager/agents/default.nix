{config, ...}: {
  home.file.".claude/CLAUDE.md" = {
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home-manager/agents/AGENTS.md";
    force = true;
  };
}
