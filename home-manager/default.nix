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

  pkgs = import nixpkgs {
    inherit system;
    config = {
      allowUnfree = true;
      allwoUnfreePredicate = true;
    };
    overlays = import ./overlay/default.nix {
      inherit emacs-overlay;
      inherit neovim-nightly-overlay;
    };
  };
  emacs = import ./packages/emacs {
    inherit (nixpkgs) lib;
    inherit pkgs sources;
  };
  emacsPkgs = emacs.emacs-stable;
  nodePkgs = pkgs.callPackage ../node2nix {inherit pkgs;};

  yaskkserv2 = pkgs.callPackage ../yaskkserv2 {inherit pkgs sources;};
  pkgs-textlint = pkgs.callPackage ../textlint {inherit pkgs sources;};

  defaultPrograms = import ./programs/default.nix {
    inherit pkgs;
    inherit org-babel emacsPkgs nodePkgs sources config;
  };
in {
  imports = defaultPrograms;

  home = {
    username = username;
    homeDirectory = "/Users/${username}";

    stateVersion = "25.11";

    packages = with pkgs; [
      # git
      curl
      uv
      nodejs_24
      typescript
      lua
      alejandra
      peco
      ghq
      gh
      deno
      rustc
      rustup
      ripgrep
      vim
      tree-sitter
      pyright
      ruff
      isort
      yaml-language-server
      lua-language-server
      stylua
      typescript-language-server
      nvfetcher
      udev-gothic
      ghostscript
      wezterm
      cbc
      sl
      ispell
      typst
      tdf
      perl
      nixd
      copilot-language-server
      texlab
      auctex
      hugo
      texliveBasic

      yaskkserv2
      pkgs-textlint
    ];
    file = {
      # ".config/nvim" = {
      #   target = ".config/nvim";
      #   source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home-manager/programs/neovim/nvim";
      # };
      # ".config/nix" = {
      #   target = ".config/nix";
      #   source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/";
      # };
      ".config/karabiner/karabiner.json".text = builtins.readFile ../karabiner/karabiner.json;
    };
  };
  programs.home-manager.enable = true;
}
