{pkgs, ...}: {
  programs.nixvim.plugins = {
    lsp = {
      enable = true;
      inlayHints = true;
      inline_completion.enable = true;
      servers = {
        nixd = {
          enable = true;
          settings = let
            flake = ''(buitins.getFlake "/Users/kei/dotfiles")'';
          in {
            nixpkgs = {
              expr = "import ${flake}/nixpkgs {}";
            };
            formatting = {
              command = ["nixpkgs-rfc-style"];
            };
            options = {
              nixos.expr = "(%s).homeConfigurations.myHomeConfig.options";
              home_manager.expr = "(%s).homeConfigurations.myHomeConfig.home.packages";
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
        pyright = {
          enable = true;
          rootMarkers = ["pyproject.toml" "setup.py"];
          onAttach.function = ''
            local disabled_capabilities = {
              "callHierarchyProvider",
              "codeActionProvider",
              "codeLensProvider",
              "colorProvider",
              "declarationProvider",
              "definitionProvider",
              "diagnosticProvider",
              "documentFormattingProvider",
              "documentHighlightProvider",
              "documentLinkProvider",
              "documentOnTypeFormattingProvider",
              "documentRangeFormattingProvider",
              "documentSymbolProvider",
              "executeCommandProvider",
              "foldingRangeProvider",
              "hoverProvider",
              "implementationProvider",
              "inlayHintProvider",
              "monikerProvider",
              "referencesProvider",
              "renameProvider",
              "selectionRangeProvider",
              "semanticTokensProvider",
              "signatureHelpProvider",
              "typeDefinitionProvider",
              "typeHierarchyProvider",
              "workspaceSymbolProvider",
            }

            for _, capability in ipairs(disabled_capabilities) do
              client.server_capabilities[capability] = false
            end
          '';
          extraOptions = {
            settings = {
              pyright = {
                disableOrganizeImports = true;
              };
              python = {
                analysis = {
                  autoImportCompletions = true;
                  diagnosticMode = "openFilesOnly";
                  exclude = ["**"];
                  ignore = ["*"];
                  typeCheckingMode = "off";
                };
                venvPath = ".";
                pythonPath = ".venv/bin/python";
              };
            };
          };
        };
        denols = {
          enable = true;
          root_markers = ["deno.json" "deno.jsonc"];
          init_options = {
            lint = true;
            unstable = false;
            suggest.imports.hosts = ["https://deno.land" "https://cdn.nest.land" "https://crux.land"];
          };
        };
        ts_ls = {
          enable = true;
          filetypes = ["typescript" "typescriptreact" "javascript" "javascriptreact"];
          root_markers = ["package.json" "tsconfig.json" "jsconfig.json"];
        };
        tinymist = {
          enable = true;
        };
        docker-language-server = {
          enable = true;
          cmd = ["docker-langserver" "--stdio"];
          filetypes = ["dockerfile"];
          root_markers = [
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
        cspell.enable = true;
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
