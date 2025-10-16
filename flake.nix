{
  description = "My flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
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
	 treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    flake-parts,
	treefmt-nix,
    ...
  } @ inputs: 
  flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];

      imports = [ treefmt-nix.flakeModule ];

      flake = {
        darwinConfigurations = {
          darwin = import ./hosts/darwin { inherit inputs; };
        };
      };

      perSystem =
        { ... }:
        {
          treefmt = {
            projectRootFile = "flake.nix";
            programs = {
              actionlint.enable = true;
              nixfmt.enable = true;
              taplo.enable = true;
              jsonfmt.enable = true;
              yamlfmt.enable = true;
              fish_indent.enable = true;
              stylua.enable = true;
              shfmt.enable = true;
              prettier.enable = true;
            };
          };
        };
    };
}
