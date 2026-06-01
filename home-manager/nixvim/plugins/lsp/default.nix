{
  lib,
  pkgs,
  ...
}: {
  plugins = {
    lsp = {
      enable = true;
      inlayHints.enable = true;
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
          root_markers = ["pyproject.toml" "setup.py"];
          settings = {
            pyright = {
              disableOrganizeImports = true;
            };
            python = {
              analysis.ignore = ["**/venv/**" "**/.venv/**" "**/env/**" "**/.env/**"];
              venvPath = ".";
              pythonPath = ".venv/bin/python";
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
          cmd = "tinymist";
          filetypes = ["typst"];
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
        rust_analyzer.enable = true;
        copilot.enable = true;
        hls.enable = true;
      };
      keymaps = {
        silent = true;
        diagnostic = {
          "[e" = "goto_prev";
          "]e" = "goto_next";
        };
        lspBuf = {
          "gD" = "declaration";
          "gd" = "goto_definition";
          "gi" = "goto_implementation";
          "gr" = "references";
          "H" = "hover";
          "K" = "hover";
          "<leader>D" = "type_definition";
          "<leader>rn" = "rename";
        };
      };
    };
  };
}
