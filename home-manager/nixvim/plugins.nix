{pkgs, ...}: {
  programs.nixvim.plugins = {
    mini.enable = true;
    repeat.enable = true;
    # lspconfig.enable = true;
    no-neck-pain.enable = true;

    conform-nvim = {
      enable = true;
      # callSetup = false;
settings = {
      formatters_by_ft = {
        lua = ["stylua"];
        python = ["ruff" "ruff_organize_imports" "ruff_format" "ruff_lint"];
        nix = ["alejandra"];
        rust = ["rustfmt"];
      };
format_on_save = {
      lsp_format = "fallback";
      timeout_ms = 500;
    };
    notify_on_error = true;
};
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
