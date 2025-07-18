{
  description = "My flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
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
  };

  outputs = {
    self,
    nixpkgs,
    nix-darwin,
    home-manager,
    emacs-overlay,
    org-babel,
    neovim-nightly-overlay,
    ...
  } @ inputs: let
    system = "aarch64-darwin";
    pkgs = import nixpkgs {inherit system;};
  in {
    darwinConfigurations.kei-darwin = nix-darwin.lib.darwinSystem {
      system = system;
      modules = [
        ./nix-darwin/default.nix
      ];
    };

    formatter.${system} = "alejandra";
    homeConfigurations = {
      myHomeConfig = home-manager.lib.homeManagerConfiguration {
        pkgs = pkgs;
        extraSpecialArgs = {
          inherit nixpkgs home-manager emacs-overlay org-babel system neovim-nightly-overlay;
          inherit (home-manager.lib) homeManagerConfiguration;
        };
        modules = [
          ./home-manager/default.nix
        ];
      };
    };
    apps.${system}.update = {
      type = "app";
      program = toString (pkgs.writeShellScript "update-script" ''
                  set -e
                  echo "updating flake..."
                  nix flake update
                  echo "Updating home-manager..."
                  nix run nixpkgs#home-manager -- switch --flake .#myHomeConfig
                  echo "Updating nix-darwin..."
                  sudo nix run nix-darwin -- switch --flake .#kei-darwin
                  echo "update complete"
        #           alejandra .
        # nix store gc
                  echo "done!!!"
      '');
    };
  };
}
