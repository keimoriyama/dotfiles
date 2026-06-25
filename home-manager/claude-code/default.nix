{config, ...}: {
  home.file.".claude/settings.json" = {
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home-manager/claude-code/settings.json";
    force = true;
  };
}
