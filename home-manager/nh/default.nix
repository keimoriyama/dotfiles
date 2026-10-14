{config, ...}: {
  programs.nh = {
    enable = true;
    flake = "${config.home.homeDirectory}/ghq/github.com/keimoriyama/dotfiles";
    clean = {
      enable = true;
      extraArgs = "--keep-since 7d --keep 5";
    };
  };
}
