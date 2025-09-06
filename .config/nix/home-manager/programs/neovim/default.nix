{
  pkgs,
  sources,
}: let
  dpp-vim = pkgs.vimUtils.buildVimPlugin {
    pname = "dpp-vim";
    src = sources.dpp-vim.src;
	version = sources.dpp-vim.version;
    dontBuild = true;
  };
in {
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      dpp-vim
	  denops-vim
    ];
  };
}
