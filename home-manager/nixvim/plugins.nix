{pkgs, ...}: {
  plugins = {
    mini.enable = true;
    repeat.enable = true;
    lspconfig.enable = true;
    no-neck-pain.enable = true;

    conform-nvim = {
      enable = true;
      callSetup = false;
    };

    hlchunk = {
      enable = true;
      callSetup = false;
    };

    lastplace = {
      enable = true;
      callSetup = false;
    };

    toggleterm = {
      enable = true;
      callSetup = false;
    };

    treesitter = {
      enable = true;
      nixGrammars = false;
      luaConfig.content = pkgs.lib.mkForce "";
    };

    treesitter-context = {
      enable = true;
      callSetup = false;
    };

    web-devicons = {
      enable = true;
      callSetup = false;
    };
  };
}
