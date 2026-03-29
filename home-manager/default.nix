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
  llm-agents,
  arto,
  ...
}: let
  sources = pkgs.callPackage ../_sources/generated.nix {};

  pkgs = import nixpkgs {
    inherit system;
    config = {
      allowUnfree = true;
      allwoUnfreePredicate = true;
      problems.handlers = {
        nss_wrapper.broken = "warn";
      };
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
  llmAgentsPkgs = llm-agents.packages.${system};
  artoPkg =
    if pkgs.stdenv.isDarwin
    then arto.packages.${system}.default
    else null;
  # nodePkgs = pkgs.callPackage ../node2nix {inherit pkgs;};
  yaskkserv2 = pkgs.callPackage ./yaskkserv2 {inherit pkgs sources;};
  # mocword = pkgs.callPackage ./mocword {inherit pkgs sources;};
  cargo-compete = pkgs.callPackage ./cargo-compete {inherit pkgs sources;};
  # rassumfrassum = pkgs.callPackage ../rassumfrassum {inherit pkgs;};

  wezterm-config = import ./wezterm {inherit pkgs;};
  emacs-config = import ./emacs {
    inherit
      pkgs
      emacsPkgs
      org-babel
      sources
      ;
  };
  fish-config = import ./fish {inherit pkgs sources;};
  # neovim-config = import ./neovim {inherit pkgs sources config;};
  git-config = import ./git;

  utils = import ./utils.nix {inherit pkgs;};
  langs = import ./langs.nix {inherit pkgs;};
  darwin =
    if pkgs.stdenv.isDarwin
    then import ./darwin.nix {inherit pkgs;}
    else [];
  gui = import ./gui.nix {inherit pkgs;};
  llm-agent-pkgs = import ./llm-agent-pkg.nix {
    inherit llmAgentsPkgs;
  };
in {
  imports = [
    wezterm-config
    fish-config
    emacs-config
    git-config
    # neovim-config
  ];

  programs.home-manager.enable = true;
  home = {
    stateVersion = "25.11";
    username = username;
    homeDirectory = "/Users/${username}";

    packages = with pkgs;
      [
        #editor & other tools
        vim
        tree-sitter
        # mocword
        terminal-notifier
        # online-judge-tools
        # online-judge-template-generator
        cargo-compete
        yaskkserv2
        artoPkg
      ]
      ++ utils
      ++ langs
      ++ gui
      ++ llm-agent-pkgs
      ++ lib.optionals stdenv.isLinux [
        zoom-us
      ]
      ++ darwin;
    file = {
      ".skk-dict/SKK-JISYO.L".source = "${pkgs.skkDictionaries.l}/share/skk/SKK-JISYO.L";
      "Library/Containers/net.mtgto.inputmethod.macSKK/Data/Documents/Settings/kana-rule.conf".source = ./macskk/kana-rule.conf;
    };
    activation.trampolineApps = home-manager.lib.hm.dag.entryAfter ["writeBoundary"] ''
      ${builtins.readFile ./trampoline-apps.sh}
      fromDir="$HOME/Applications/Home Manager Apps"
      toDir="$HOME/Applications/Home Manager Trampolines"
      sync_trampolines "$fromDir" "$toDir"
    '';
  };
}
