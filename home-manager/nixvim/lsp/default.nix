{
  pkgs,
  config,
  ...
}: {
  programs.nixvim.plugins = {
    lsp = {
      enable = true;
      inlayHints = true;
      servers = {
        nixd = {
          enable = true;
          settings = let
            flake = ''(builtins.getFlake "${config.home.homeDirectory}/dotfiles")'';
            darwin = "${flake}.darwinConfigurations.my-config";
          in {
            nixpkgs = {
              expr = "import ${flake}.inputs.nixpkgs {}";
            };
            formatting = {
              command = ["alejandra"];
            };
            options = {
              nix-darwin.expr = "${darwin}.options";
              home-manager.expr = "${darwin}.options.home-manager.users.type.getSubOptions []";
            };
          };
        };
        lua_ls = {
          enable = true;
          settings.lua = {
            diagnostics = {
              globals = ["vim" "hs" "wez"];
            };
            semantic.enable = true;
          };
        };
        denols = {
          enable = true;
          rootMarkers = ["deno.json" "deno.jsonc"];
          extraOptions = {
            init_options = {
              lint = true;
              unstable = false;
              suggest.imports.hosts = ["https://deno.land" "https://cdn.nest.land" "https://crux.land"];
            };
          };
        };
        ts_ls = {
          enable = true;
          filetypes = ["typescript" "typescriptreact" "javascript" "javascriptreact"];
          rootMarkers = ["package.json" "tsconfig.json" "jsconfig.json"];
        };
        tinymist = {
          enable = true;
        };
        docker_language_server = {
          enable = true;
          cmd = ["docker-langserver" "--stdio"];
          filetypes = ["dockerfile"];
          rootMarkers = [
            "Dockerfile"
            "docker-compose.yaml"
            "docker-compose.yml"
            "compose.yaml"
            "compose.yml"
            "docker-bake.json"
            "docker-bake.hcl"
            "docker-bake.override.json"
            "docker-bake.override.hcl"
          ];
        };
        ruff.enable = true;
        ty.enable = true;
        rust_analyzer = {
          enable = true;
          installRustc = false;
          installCargo = false;
        };
        hls = {
          enable = true;
          installGhc = false;
        };
        copilot = {
          enable = true;
          package = pkgs.copilot-language-server;
        };
        texlab.enable = true;
        efm = {
          enable = true;
          filetypes = ["lua" "python" "nix" "rust" "markdown" "org" "tex" "text"];
          extraOptions = {
            init_options = {
              documentFormatting = true;
              documentRangeFormatting = true;
            };
          };
          settings = {
            rootMarkers = [".git/"];
            languages = {
              lua = [
                {
                  formatCommand = "${pkgs.stylua}/bin/stylua -";
                  formatStdin = true;
                }
              ];
              python = [
                {
                  formatCommand = "${pkgs.ruff}/bin/ruff format -";
                  formatStdin = true;
                }
              ];
              nix = [
                {
                  formatCommand = "${pkgs.alejandra}/bin/alejandra";
                  formatStdin = true;
                }
              ];
              rust = [
                {
                  formatCommand = "rustfmt";
                  formatStdin = true;
                }
              ];
              markdown = [
                {
                  lintCommand = "textlint --stdin --stdin-filename \${INPUT} --format unix";
                  lintStdin = true;
                  lintFormats = ["%f:%l:%c: %m [%trror/%r]" "%f:%l:%c: %m [%tarning/%r]"];
                }
              ];
              org = [
                {
                  lintCommand = "textlint --stdin --stdin-filename \${INPUT} --format unix";
                  lintStdin = true;
                  lintFormats = ["%f:%l:%c: %m [%trror/%r]" "%f:%l:%c: %m [%tarning/%r]"];
                }
              ];
              tex = [
                {
                  lintCommand = "textlint --stdin --stdin-filename \${INPUT} --format unix";
                  lintStdin = true;
                  lintFormats = ["%f:%l:%c: %m [%trror/%r]" "%f:%l:%c: %m [%tarning/%r]"];
                }
              ];
              text = [
                {
                  lintCommand = "textlint --stdin --stdin-filename \${INPUT} --format unix";
                  lintStdin = true;
                  lintFormats = ["%f:%l:%c: %m [%trror/%r]" "%f:%l:%c: %m [%tarning/%r]"];
                }
              ];
            };
          };
        };
      };
      keymaps = {
        silent = true;
        diagnostic = {
          "[e" = {action = "goto_prev";};
          "]e" = {action = "goto_next";};
        };
        lspBuf = {
          "gD" = {action = "declaration";};
          "gd" = {action = "definition";};
          "gi" = {action = "implementation";};
          "gr" = {action = "references";};
          "H" = {action = "hover";};
          "<leader>D" = {action = "type_definition";};
          "<leader>rn" = {action = "rename";};
        };
      };
    };
  };

  programs.nixvim.extraConfigLua = builtins.readFile ./extraConfig.lua;
}
