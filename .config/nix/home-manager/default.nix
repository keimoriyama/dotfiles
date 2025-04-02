{
  nixpkgs,
  home-manager,
  emacs-overlay,
  org-babel,
  system,
  neovim-nightly-overlay,
  ...
}: let
  username = "kei";

  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = import ./overlay/default.nix {
      inherit emacs-overlay;
      inherit neovim-nightly-overlay;
    };
  };
  emacs = import ./packages/emacs {
    inherit (nixpkgs) lib;
    inherit pkgs;
  };
  python = import ./packages/python {
    inherit (nixpkgs) lib;
    inherit pkgs;
  };
  emacsPkg = emacs.emacs-stable;
  defaultPrograms = import ./programs/default.nix {
    inherit pkgs;
    inherit org-babel emacsPkg;
  };
  sources = pkgs.callPackage ../_sources/generated.nix {};
in {
  imports = defaultPrograms;

  home = {
    username = username;
    homeDirectory = "/Users/${username}";

    stateVersion = "25.05";
    # packages = defaultPackages;
    packages = with pkgs; [
      git
      curl
      uv
      nodejs_23
      python313
      alejandra
      fish
      fishPlugins.z
      deno
      wezterm
      ripgrep
      neovim
      vim
      tree-sitter
      pyright
      ruff
      yaml-language-server
      lua-language-server
    ];
  };
  programs.home-manager.enable = true;
}
