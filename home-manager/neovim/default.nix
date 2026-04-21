{
  pkgs,
  sources,
  config,
  home-manager,
}: let
  # plugins = pkgs.callPackage ./plugins.nix {inherit pkgs sources;};
  # dpp_ext_dir = ".cache/dpp/nix_plugins";
in {
  home.activation.neovimConfig = home-manager.lib.hm.dag.entryAfter ["writeBoundary"] ''
    run mkdir -p "$HOME/.config"
    run rm -rf "$HOME/.config/nvim"
    run ln -s "${config.home.homeDirectory}/dotfiles/home-manager/neovim/nvim" "$HOME/.config/nvim"
  '';
  programs.neovim = {
    defaultEditor = true;
    enable = true;
    withPython3 = false;
    withRuby = false;
  };
}
