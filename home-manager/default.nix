{
  nixpkgs,
  config,
  home-manager,
  emacs-overlay,
  org-babel,
  system,
  neovim-nightly-overlay,
  username,
  brew-nix,
  ...
}: let
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
      inherit brew-nix;
    };
  };
  emacs = import ./packages/emacs {
    inherit (nixpkgs) lib;
    inherit pkgs sources;
  };
  emacsPkgs = emacs.emacs-stable;
  # nodePkgs = pkgs.callPackage ../node2nix {inherit pkgs;};

  yaskkserv2 = pkgs.callPackage ./yaskkserv2 {inherit pkgs sources;};
  mocword = pkgs.callPackage ./mocword {inherit pkgs sources;};
  cargo-compete = pkgs.callPackage ./cargo-compete {inherit pkgs sources;};
  # rassumfrassum = pkgs.callPackage ../rassumfrassum {inherit pkgs;};

  defaultPrograms = import ./programs/default.nix {
    inherit pkgs;
    inherit
      org-babel
      emacsPkgs
      sources
      config
      ;
  };
in {
  imports = defaultPrograms;

  programs.home-manager.enable = true;
  home = {
    stateVersion = "25.11";
    username = username;
    homeDirectory = "/Users/${username}";

    packages = with pkgs;
      [
        curl
        rsync
        uv
        nodejs_24
        typescript
        lua
        alejandra
        peco
        ghq
        gh
        deno
        # rustc
        rustup
        cargo-generate
        ripgrep
        vim
        tree-sitter

        basedpyright
        ruff
        ty
        isort
        yaml-language-server
        lua-language-server
        stylua
        typescript-language-server
        udev-gothic
        ghostscript
        wezterm
        cbc
        sl
        ispell
        typst
        tdf
        perl
        nil
        nixd
        copilot-language-server
        texlab
        auctex

        nvfetcher
        hugo
        texliveFull
        fzf
        docker
        docker-language-server
        yaml-language-server
        tinymist
        typstyle
        go
        prettier

        online-judge-tools
        online-judge-template-generator
        cargo-compete

        yaskkserv2
        slack
        discord
        spotify
        wezterm
        mocword
        notion-app
        google-chrome
        wget
        tree
        (textlint.withPackages [
          textlint-rule-preset-ja-technical-writing
          textlint-plugin-org
          textlint-plugin-latex2e
        ])

        # rassumfrassum
        codex
      ]
      ++ lib.optionals stdenv.isLinux [
        zoom-us
      ]
      ++ lib.optionals stdenv.isDarwin [
        brewCasks.zoom
        brewCasks.skim
        brewCasks.docker-desktop
        brewCasks.chatgpt
        brewCasks.chatgpt-atlas
        # (brewCasks.steam.overrideAttrs
        #   (oldAttrs: {
        #     src = pkgs.fetchurl {
        #       url = builtins.head oldAttrs.src.urls;
        #       hash = "sha256-X1VnDJGv02A6ihDYKhedqQdE/KmPAQZkeJHudA6oS6M=";
        #     };
        #   }))
      ];
    file = {
      ".skk-dict/SKK-JISYO.L".source = "${pkgs.skkDictionaries.l}/share/skk/SKK-JISYO.L";
      "~/Library/Containers/net.mtgto.inputmethod.macSKK/Data/Documents/Settings/kana-rule.conf".source = ./macskk/kana-rule.conf;
    };
    activation.trampolineApps = home-manager.lib.hm.dag.entryAfter ["writeBoundary"] ''
      ${builtins.readFile ./trampoline-apps.sh}
      fromDir="$HOME/Applications/Home Manager Apps"
      toDir="$HOME/Applications/Home Manager Trampolines"
      sync_trampolines "$fromDir" "$toDir"
    '';
  };
}
