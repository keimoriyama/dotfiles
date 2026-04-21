{
  pkgs,
  sources,
  config,
}: let
  # plugins = pkgs.callPackage ./plugins.nix {inherit pkgs sources;};
  # dpp_ext_dir = ".cache/dpp/nix_plugins";
in {
  home.file = {
    ".config/nvim" = {
      target = ".config/nvim";
      source = config.lib.file.mkOutOfStoreSymlink ./nvim;
    };
  };
  programs.neovim = {
    defaultEditor = true;
    enable = true;
  };
}
