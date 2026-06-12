{pkgs, ...}: {
  programs.nixvim.plugins = {
    lz-n.enable = true;

    mini = {
      callSetup = true;
      enable = true;
      lazyLoad.settings.event = "VimEnter";
      mock_nvim_web_devicons = true;
      # mockDevIcons = true;
      modules = {
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
            {
              mode = "n";
              keys = "s";
            }
          ];
          clues = [
            {__raw = "require(\"mini.clue\").gen_clues.square_brackets()";}
            {__raw = "require(\"mini.clue\").gen_clues.builtin_completion()";}
            {__raw = "require(\"mini.clue\").gen_clues.g()";}
            {__raw = "require(\"mini.clue\").gen_clues.marks()";}
            {__raw = "require(\"mini.clue\").gen_clues.registers()";}
            {__raw = "require(\"mini.clue\").gen_clues.windows()";}
            {__raw = "require(\"mini.clue\").gen_clues.z()";}
          ];
        };
        completion = {
          lsp_completion = {
            source_func = "omnifunc";
            auto_setup = true;
          };
        };
        icons = {};
        pick = {};
        extra = {};
        git = {};
        notify = {};
        statusline = {use_icons = true;};
        trailspace = {};
        snippets = {
          mappings = {
            expand = "<C-k>";
          };
        };
        pairs = {};
        surround = {};
        jump = {};
      };
    };
    repeat.enable = true;
    no-neck-pain = {
      enable = true;
      lazyLoad.settings.cmd = "NoNeckPain";
    };

    hlchunk = {
      enable = true;
    };

    lastplace = {
      enable = true;
    };

    toggleterm = {
      lazyLoad.settings = {
        keys = [
          "<C-t>"
        ];
      };
      enable = true;
      settings = {
        direction = "float";
        open_mapping = "[[<c-t>]]";
      };
    };

    treesitter = {
      # lazyLoad.settings.cmd = "VimEnter";
      enable = true;
      highlight.enable = true;
      indent.enable = true;
      luaConfig.content = pkgs.lib.mkForce "";
    };

    treesitter-context = {
      # lazyLoad.settings.cmd = "VimEnter";
      enable = true;
      settings = {
        min_window_height = 20;
      };
    };
    context_vt = {
      enable = true;
      settings = {
        min_rows = 5;
      };
    };

    web-devicons = {
      enable = true;
    };
    vimtex = {
      enable = true;
      lazyLoad.settings.ft = "tex";
      settings = {
        view_method = "skim";
      };
    };
  };
}
