{
  pkgs,
  nixvim,
  ...
}: let
  sonicTemplateDir = toString ./nvim/template;
in {
  imports = [nixvim.homeModules.nixvim];

  programs.nixvim = {
    enable = true;
    defaultEditor = true;

    extraPlugins = with pkgs.vimPlugins; [
      mini-nvim
      catppuccin-nvim
      denops-vim
      vim-repeat
      nvim-lastplace
      hlchunk-nvim
      nvim-treesitter
      nvim-treesitter-context
      toggleterm-nvim
      nvim-lspconfig
      conform-nvim
      nvim-web-devicons
      vimtex
      incline-nvim
    ];

    extraConfigLuaPre = ''
      vim.loader.enable()
      vim.g.sonictemplate_key = 0
      vim.g.sonictemplate_intelligent_key = 0
      vim.g.sonictemplate_postfix_key = 0
      vim.g.sonictemplate_vim_template_dir = "${sonicTemplateDir}"
      vim.g.gf_improved_no_mappings = 1
      vim.g.typst_pdf_viewer = "tdf"
    '';

    extraConfigLua = builtins.readFile ./nixvim/init.lua;
  };
}
