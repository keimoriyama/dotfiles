{
  pkgs,
  nixvim,
  ...
}: let
  sonicTemplateDir = toString ./nvim/template;
  plugins = import ./plugins.nix {inherit pkgs;};
  lsp = import ./plugins/lsp {inherit pkgs;};
  # keymapconfig = import ./keymapconfig.nix {};
  option = import ./option.nix {inherit pkgs;};
in {
  imports = [
    nixvim.homeModules.nixvim
    plugins
    lsp
    # keymapconfig
    option
  ];

  programs.nixvim = {
    enable = true;
    defaultEditor = true;

    lasyLoad.enable = true;
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
    globals.mapleader = ";";
    extraConfigLua = builtins.readFile ./init.lua;
    colorschemes.catppuccin.enable = true;

    keymaps = [
      {
        mode = "n";
        key = "<Esc><Esc>";
        action = ":<C-u>set nohlsearch<Return>";
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<Leader>w";
        action = ":w<CR>";
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<Leader>q";
        action.__raw = ''
          function()
            if vim.api.nvim_buf_get_name(0) == "" then
              vim.api.nvim_command("q")
            else
              vim.api.nvim_command("wq")
            end
          end
        '';
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<Leader>Q";
        action = ":q!<CR>";
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "+";
        action = "<C-a>";
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "-";
        action = "<C-x>";
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "v";
        key = "<Leader>cw";
        action = "g<C-G>";
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<C-x>2";
        action = "<C-w>s";
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<C-x>3";
        action = "<C-w>v";
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "t";
        key = "<Esc>";
        action = "<C-\\><C-n>";
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<Leader>ff";
        action.__raw = ''
          function()
            require("mini.pick").builtin.files()
          end
        '';
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>h";
        action.__raw = ''
          function()
            require("mini.pick").builtin.help()
          end
        '';
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>fr";
        action.__raw = ''
          function()
            require("mini.pick").builtin.grep_live()
          end
        '';
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>lr";
        action.__raw = ''
          function()
            require("mini.extra").pickers.lsp({ scope = "references" })
          end
        '';
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>ld";
        action.__raw = ''
          function()
            require("mini.extra").pickers.diagnostic({ scope = "current" })
          end
        '';
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>ls";
        action.__raw = ''
          function()
            require("mini.extra").pickers.lsp({ scope = "document_symbol" })
          end
        '';
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>ic";
        action.__raw = ''
          function()
            vim.lsp.buf.incoming_calls()
          end
        '';
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>oc";
        action.__raw = ''
          function()
            vim.lsp.buf.outgoing_calls()
          end
        '';
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<Leader>sb";
        action.__raw = ''
          function()
            require("mini.pick").builtin.buffers()
          end
        '';
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "/";
        action.__raw = ''
          function()
            require("mini.extra").pickers.buf_lines({ scope = "current", preserve_order = false })
          end
        '';
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "*";
        action.__raw = ''
          function()
            local pick = require("mini.pick")
            local query = vim.fn.expand("<cword>")
            if query ~= nil and query ~= "" then
              vim.schedule(function()
                if pick.is_picker_active() then
                  pick.set_picker_query({ query })
                end
              end)
            end
            require("mini.extra").pickers.buf_lines({ scope = "current", preserve_order = false })
          end
        '';
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>k";
        action.__raw = ''
          function()
            require("mini.extra").pickers.keymaps()
          end
        '';
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>dp";
        action = "<cmd>DepsUpdate<cr>";
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "n";
        action.__raw = ''
          function()
            require("mini.pick").builtin.resume()
          end
        '';
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>e";
        action.__raw = ''
          function()
            require("mini.extra").pickers.diagnostic({ scope = "current" })
          end
        '';
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "n";
        key = "<leader>sf";
        action.__raw = ''
          function()
            local path = vim.api.nvim_buf_get_name(0)
            if path == "" then
              path = nil
            end
            require("mini.files").open(path)
          end
        '';
        options = {
          noremap = true;
          silent = true;
        };
      }
      {
        mode = "i";
        key = "<C-g>";
        action = "copilot#Accept()";
        options = {
          expr = true;
          silent = true;
        };
      }
    ];
  };
}
