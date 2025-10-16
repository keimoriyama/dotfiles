{inputs}: let
  inherit (inputs) nix-darwin home-manager;
  inherit (inputs) nixpkgs;
  system = "aarch64-darwin";
  pkgs = import nixpkgs {inherit system;};
  username = "kei";
  configuration = {...}: {
    users.users.${username}.home = "/Users/${username}";
  };
in
  nix-darwin.lib.darwinSystem {
    inherit pkgs system;
    inherit (inputs.nixpkgs) lib;
    specialArgs = {
      inherit username pkgs;
    };
    modules = [
      configuration
      ../../nix-darwin/default.nix
      home-manager.darwinModules.home-manager
      {
        home-manager = {
          useUserPackages = true;
          users."${username}" = import ../../home-manager/default.nix;
          extraSpecialArgs = {
            inherit nixpkgs home-manager system;
            inherit (home-manager.lib) homeManagerConfiguration;
            inherit (inputs) emacs-overlay org-babel neovim-nightly-overlay;
          };
        };
      }
    ];
  }
