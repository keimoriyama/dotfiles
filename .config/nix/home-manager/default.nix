{
  nixpkgs,
  config,
  home-manager,
  emacs-overlay,
  org-babel,
  system,
  neovim-nightly-overlay,
  ...
}: let
  username = "kei";
  sources = pkgs.callPackage ../_sources/generated.nix {};
  lib = nixpkgs.lib;

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
    inherit pkgs sources;
  };
  python = import ./packages/python {
    inherit (nixpkgs) lib;
    inherit pkgs;
  };
  programs.fish.enable = true;

  emacsPkg = emacs.emacs-stable-without-nativecomp;
  defaultPrograms = import ./programs/default.nix {
    inherit pkgs;
    inherit org-babel emacsPkg;
  };
in {
  imports = defaultPrograms;

  home = {
    username = username;
    homeDirectory = "/Users/${username}";

    stateVersion = "25.05";

    packages = with pkgs; [
      git
      curl
      uv
      nodejs_23
      typescript
      python313
      lua
      alejandra
      peco
      ghq
      gh
      deno
      ripgrep
      neovim
      vim
      tree-sitter
      pyright
      ruff
      yaml-language-server
      lua-language-server
      typescript-language-server
      docker
      nvfetcher
      udev-gothic
    ];
    file = {
      ".config/nvim" = {
        target = ".config/nvim";
        source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/.config/nvim";
      };
      ".config/nix" = {
        target = ".config/nix";
        source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/.config/nix";
      };
      ".config/karabiner/karabiner.json".text = builtins.readFile ../../karabiner/karabiner.json;
    };
  };

  programs.git = {
    enable = true;
    userName = "keimoriyama";
    userEmail = "keischwiiz@gmail.com";
  };

  programs.home-manager.enable = true;
}
