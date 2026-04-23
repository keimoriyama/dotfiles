{
  pkgs,
  sources,
  config,
  home-manager,
}: {
  # plugins = pkgs.callPackage ./plugins.nix {inherit pkgs sources;};
  # dpp_ext_dir = ".cache/dpp/nix_plugins";
  home.file.".config/nvim" = {
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home-manager/neovim/nvim";
    force = true;
  };

  programs.neovim = {
    defaultEditor = true;
    enable = true;
    withPython3 = false;
    withRuby = false;
  };
}
