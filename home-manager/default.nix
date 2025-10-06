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
      skktools

      # textlint
      # textlint-plugin-org
      # textlint-plugin-latex2e
      # textlint-rule-preset-ja-technical-writing
      # textlint-rule-write-good

      yaskkserv2
    ];
    file = {
      ".config/karabiner/karabiner.json".text = builtins.readFile ../karabiner/karabiner.json;
      ".skk-dict/SKK-JISYO.L".source = "${pkgs.skkDictionaries.l}/share/skk/SKK-JISYO.L";
    };
  };
  programs.home-manager.enable = true;
}
