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
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
    nixos-wsl,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} ({self, ...}: let
      darwinSystem = "aarch64-darwin";
      nixosSystem = "x86_64-linux";
      username = "kei";
      homeModules = [./home-manager/default.nix];
      darwinSpecialArgs = {
        inherit
          nixpkgs
          home-manager
          emacs-overlay
          org-babel
          neovim-nightly-overlay
          username
          brew-nix
          llm-agents
          arto
          nixos-wsl
          ;
        system = darwinSystem;
        inherit (home-manager.lib) homeManagerConfiguration;
      };
      nixosSpecialArgs = {
        inherit
          nixpkgs
          home-manager
          emacs-overlay
          org-babel
          neovim-nightly-overlay
          username
          brew-nix
          llm-agents
          arto
          nixos-wsl
          ;
        system = nixosSystem;
        inherit (home-manager.lib) homeManagerConfiguration;
      };
    in {
      systems = [darwinSystem nixosSystem];

      perSystem = {pkgs, ...}: {
        apps.update = {
          type = "app";
          program = toString (pkgs.writeShellScript "update-script" ''
            set -e
            echo "Updating nix-darwin and home-manager..."
            sudo env HOME="$HOME" USER="$USER" LOGNAME="$LOGNAME" \
              nix run nix-darwin -- switch --flake ${self.outPath}#kei-darwin
            echo "update complete"
          '');
        };

        formatter = pkgs.alejandra;
      };

      flake = {
        darwinConfigurations.kei-darwin = nix-darwin.lib.darwinSystem {
          system = darwinSystem;
          specialArgs = darwinSpecialArgs;
          modules = [
            ./hosts/darwin/default.nix
            home-manager.darwinModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = false;
                useUserPackages = true;
                extraSpecialArgs = darwinSpecialArgs;
                users.${username} = {
                  imports = homeModules;
                };
              };
            }
          ];
        };

        homeConfigurations.myHomeConfig = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {system = darwinSystem;};
          extraSpecialArgs = darwinSpecialArgs;
          modules = homeModules;
        };

        nixosConfigurations.nixos-wsl = nixpkgs.lib.nixosSystem {
          system = nixosSystem;
          specialArgs = nixosSpecialArgs;
          modules = [
            nixos-wsl.nixosModules.wsl
            ./hosts/nixos-wsl/default.nix
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = nixosSpecialArgs;
                users.${username} = {
                  imports = homeModules;
                };
              };
            }
          ];
        };
      };
    });
}
