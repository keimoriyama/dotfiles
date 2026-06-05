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
    extraConfig = builtins.readFile ./extraConfig.lua;
  };
}
