{
  description = "My flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    vim-src = {
      url = "github:vim/vim";
      flake = false;
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    org-babel.url = "github:emacs-twist/org-babel";
    flake-utils.url = "github:numtide/flake-utils";
    flake-parts.url = "github:hercules-ci/flake-parts";
    brew-nix = {
      url = "github:BatteredBunny/brew-nix";
      inputs.nix-darwin.follows = "nix-darwin";
      inputs.brew-api.follows = "brew-api";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    brew-api = {
      url = "github:BatteredBunny/brew-api";
      flake = false;
    };
    llm-agents.url = "github:numtide/llm-agents.nix";
    arto.url = "github:arto-app/Arto";
  };

  outputs = inputs @ {
    flake-parts,
    nixpkgs,
    nix-darwin,
    home-manager,
    emacs-overlay,
    org-babel,
    neovim-nightly-overlay,
    brew-nix,
    llm-agents,
    arto,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} ({self, ...}: let
      system = "aarch64-darwin";
      username = "kei";
      specialArgs = {
        inherit
          nixpkgs
          home-manager
          emacs-overlay
          org-babel
          system
          neovim-nightly-overlay
          username
          brew-nix
          llm-agents
          arto
          ;
        inherit (home-manager.lib) homeManagerConfiguration;
      };
    in {
      systems = [system];

      perSystem = {pkgs, ...}: {
        apps.update = {
          type = "app";
          program = toString (pkgs.writeShellScript "update-script" ''
            set -e
            echo "Updating nix-darwin and home-manager..."
            sudo nix run nix-darwin -- switch --flake ${self.outPath}#kei-darwin
            echo "update complete"
          '');
        };

        formatter = pkgs.alejandra;
      };

      flake = {
        darwinConfigurations.kei-darwin = nix-darwin.lib.darwinSystem {
          inherit system specialArgs;
          modules = [
            ./hosts/darwin/default.nix
            home-manager.darwinModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = false;
                useUserPackages = true;
                extraSpecialArgs = specialArgs;
                users.${username} = {
                  imports = [./home-manager/default.nix];
                };
              };
            }
          ];
        };

        homeConfigurations.myHomeConfig = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {inherit system;};
          extraSpecialArgs = specialArgs;
          modules = [
            ./home-manager/default.nix
          ];
        };
      };
    });
}
