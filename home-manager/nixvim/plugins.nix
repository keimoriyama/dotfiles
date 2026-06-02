{pkgs, ...}: {
  programs.nixvim.plugins = {
    mini = {
      enable = true;
      settings = {
        diff = {
          style = "vim.go.number and \"number\" or \"sign\"";
        };
        clue = {
          triggers = [
            {
              mode = "n";
              keys = "<Leader>";
            }
            {
              mode = "x";
              keys = "<Leader>";
            }
            {
              mode = "n";
              keys = "[";
            }
            {
              mode = "n";
              keys = "]";
            }
            {
              mode = "i";
              keys = "<C-x>";
            }
            {
              mode = "n";
              keys = "g";
            }
            {
              mode = "x";
              keys = "g";
            }
            {
              mode = "n";
              keys = "'";
            }
            {
              mode = "x";
              keys = "'";
            }
            {
              mode = "n";
              keys = "`";
            }
            {
              mode = "x";
              keys = "`";
            }
            {
              mode = "n";
              keys = "\"";
            }
            {
              mode = "x";
              keys = "\"";
            }
            {
              mode = "i";
              keys = "<C-r>";
            }
            {
              mode = "c";
              keys = "<C-r>";
            }
            {
              mode = "n";
              keys = "<C-w>";
            }
            {
              mode = "n";
              keys = "z";
            }
            {
              mode = "x";
              keys = "z";
            }
          ];
          clues = [
            (pkgs.lib.nixvim.nestedLiteralLua "require(\"mini.clue\").gen_clues.square_brackets()")
            (pkgs.lib.nixvim.nestedLiteralLua "require(\"mini.clue\").gen_clues.builtin_completion()")
            (pkgs.lib.nixvim.nestedLiteralLua "require(\"mini.clue\").gen_clues.g()")
            (pkgs.lib.nixvim.nestedLiteralLua "require(\"mini.clue\").gen_clues.marks()")
            (pkgs.lib.nixvim.nestedLiteralLua "require(\"mini.clue\").gen_clues.registers()")
            (pkgs.lib.nixvim.nestedLiteralLua "require(\"mini.clue\").gen_clues.windows()")
            (pkgs.lib.nixvim.nestedLiteralLua "require(\"mini.clue\").gen_clues.z()")
          ];
        };
        complention = {
          lsp_completion = {
            source_func = "omnifunc";
            auto_setup = true;
          };
        };
      };
    };
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
      settings = {
        open_mapping = "<C-t>";
        direction = "floating";
      };
    };

    treesitter = {
      enable = true;
      highlight.enable = true;
      indent.enable = true;
      luaConfig.content = pkgs.lib.mkForce "";
    };

    treesitter-context = {
      enable = true;
      callSetup = false;
      settings = {
        min_window_height = 20;
      };
    };
    context_vt = {
      enable = true;
      callSetup = false;
    };

    web-devicons = {
      enable = true;
      callSetup = false;
    };
  };
}
