{
  pkgs,
  nixvim,
  ...
}: let
  sonicTemplateDir = toString ./nvim/template;
  plugins = import ./plugins.nix {inherit pkgs;};
  lsp = import ./plugins/lsp {inherit pkgs;};
  keymap = import ./keymap.nix;
  option = import ./option.nix {inherit pkgs;};
in {
  imports = [
    nixvim.homeModules.nixvim
    plugins
    lsp
    keymap
    option
  ];

  programs.nixvim = {
    enable = true;
    defaultEditor = true;

    extraPlugins = with pkgs.vimPlugins; [
      # catppuccin-nvim
      denops-vim
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

    # extraConfigLua = builtins.readFile ./init.lua;
    colorschemes.catppuccin.enable = true;
  };
}
