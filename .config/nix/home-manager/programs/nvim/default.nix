{
  pkgs,
  sources
}:
let 
	dpp-vim  = pkgs.vimUtils.buildVimPlugin{
	pname= "dpp-vim";
	src = sources.dpp-vim.src;
	dontBuild= true;
	};
in
{

  programs.nvim= {
  enable = true;

  };
}
