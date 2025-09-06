{
  pkgs,
  sources,
}: let
  plugins = pkgs.callPackage ./plugins.nix {inherit pkgs sources;};
in {
  programs.neovim = {
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
