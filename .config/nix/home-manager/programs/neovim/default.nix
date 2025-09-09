{
  pkgs,
  sources,
  config,
}: let
  plugins = pkgs.callPackage ./plugins.nix {inherit pkgs sources;};
  dpp_ext_dir = ".cache/dpp/nix_plugins";
in {
  home.file = {
    "${dpp_ext_dir}/dpp-ext-toml" = {
      target = "${dpp_ext_dir}/dpp-ext-toml";
      source = config.lib.file.mkOutOfStoreSymlink plugins.dpp-ext-toml.outPath;
    };
    "${dpp_ext_dir}/dpp-ext-installer" = {
      target = "${dpp_ext_dir}/dpp-ext-installer";
      source = config.lib.file.mkOutOfStoreSymlink plugins.dpp-ext-installer.outPath;
    };
    "${dpp_ext_dir}/dpp-ext-lazy" = {
      target = "${dpp_ext_dir}/dpp-ext-lazy";
      source = config.lib.file.mkOutOfStoreSymlink plugins.dpp-ext-lazy.outPath;
    };
    "${dpp_ext_dir}/dpp-protocol-git" = {
      target = "${dpp_ext_dir}/dpp-protocol-git";
      source = config.lib.file.mkOutOfStoreSymlink plugins.dpp-protocol-git.outPath;
    };
  };
  programs.neovim = {
    # defaultEditor = true;
    enable = true;
    plugins = with pkgs.vimPlugins; [
      denops-vim
      plugins.dpp-vim
      plugins.dpp-ext-toml
      plugins.dpp-ext-installer
      plugins.dpp-ext-lazy
      plugins.dpp-protocol-git
    ];
  };
}
