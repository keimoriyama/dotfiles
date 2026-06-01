{
  lib,
  pkgs,
  ...
}: {
  plugins = {
    lsp = {
      enable = true;
      inlayHints = {
        enable = true;
      };
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
      };
    };
  };
}
