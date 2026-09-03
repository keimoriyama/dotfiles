{
  nixpkgs,
  config,
  home-manager,
  emacs-overlay,
  nixvim,
  org-babel,
  system,
  username,
  isWork ? false,
  brew-nix,
  llm-agents,
  arto,
  nippo,
  suiko,
  cage,
  guard-and-guide,
  cclens,
  ...
}: let
  pkgs = import nixpkgs {
    inherit system;
    config = {
      allowUnfree = true;
      allowUnfreePredicate = _: true;
    };
    overlays = import ./overlay/default.nix {
      inherit emacs-overlay;
      inherit brew-nix;
    };
  };
  sources = pkgs.callPackage ../_sources/generated.nix {};
  llmAgentsPkgs = llm-agents.packages.${system};
  artoPkg =
    if pkgs.stdenv.hostPlatform.isDarwin
    then arto.packages.${system}.default
    else null;
  # nodePkgs = pkgs.callPackage ../node2nix {inherit pkgs;};
  yaskkserv2 = pkgs.callPackage ./yaskkserv2 {inherit pkgs sources;};
  # mocword = pkgs.callPackage ./mocword {inherit pkgs sources;};
  cargo-compete = pkgs.callPackage ./cargo-compete {inherit pkgs sources;};
  kakehashi = pkgs.callPackage ./kakehashi {inherit pkgs sources;};
  nippoPkg = pkgs.callPackage ./nippo {inherit pkgs nippo;};
  suikoPkg = pkgs.callPackage ./suiko {inherit pkgs suiko;};
  # rassumfrassum = pkgs.callPackage ../rassumfrassum {inherit pkgs;};

  wezterm-config = import ./wezterm {inherit pkgs;};
  emacs-config = import ./emacs {
    inherit
      pkgs
      org-babel
      sources
      ;
  };
  fish-config = import ./fish {inherit pkgs sources;};
  nixvim-config = import ./nixvim {inherit pkgs sources config home-manager nixvim;};
  git-config = import ./git;
  nh-config = import ./nh;
  claude-code-config = import ./claude-code;
  agents-config = import ./agents;
  agent-skills-config = import ./agent-skills.nix;

  utils = import ./utils.nix {inherit pkgs;};
  langs = import ./langs.nix {inherit pkgs;};
  darwin =
    if pkgs.stdenv.hostPlatform.isDarwin
    then import ./darwin.nix {inherit pkgs;}
    else [];
  # 業務用マシンでは GUI アプリは会社の配布物を使うため home-manager では入れない。
  gui =
    if pkgs.stdenv.hostPlatform.isDarwin && !isWork
    then import ./gui.nix {inherit pkgs;}
    else [];
  llm-agent-pkgs = import ./llm-agent-pkg.nix {
    inherit llmAgentsPkgs isWork;
  };
  agent-tools = import ./agent-tools.nix {
    inherit
      system
      cage
      guard-and-guide
      cclens
      ;
  };
  basePackages = with pkgs;
    [
      # editor & other tools
      tree-sitter
      # mocword
      cargo-compete
      kakehashi
      nippoPkg
      suikoPkg
      yaskkserv2
    ]
    ++ lib.optionals (artoPkg != null) [
      artoPkg
    ];
in {
  imports = [
    wezterm-config
    fish-config
    emacs-config
    git-config
    nh-config
    #    nixvim-config
    claude-code-config
    agents-config
    agent-skills-config
  ];

  programs.home-manager.enable = true;
  home = {
    stateVersion = "26.05";
    username = username;
    homeDirectory = pkgs.lib.mkDefault (
      if pkgs.stdenv.hostPlatform.isDarwin
      then builtins.toPath "/Users/${username}"
      else builtins.toPath "/home/${username}"
    );

    sessionVariables = {
      EDITOR = "nvim";
    };

    packages =
      basePackages
      ++ utils
      ++ langs
      ++ gui
      ++ llm-agent-pkgs
      ++ agent-tools
      ++ darwin;
    file = {
      ".skk-dict/SKK-JISYO.L".source = "${pkgs.skkDictionaries.l}/share/skk/SKK-JISYO.L";
    };
    activation = pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
      trampolineApps = home-manager.lib.hm.dag.entryAfter ["writeBoundary"] ''
        ${builtins.readFile ./trampoline-apps.sh}
        fromDir="$HOME/Applications/Home Manager Apps"
        toDir="$HOME/Applications/Home Manager Trampolines"
        sync_trampolines "$fromDir" "$toDir"
      '';
      # macSKKはサンドボックスアプリのため/nix/storeへのsymlinkを辿れない。
      # コンテナ内へ実ファイルとしてコピーする必要がある。
      macskkFiles = home-manager.lib.hm.dag.entryAfter ["writeBoundary"] ''
        container="$HOME/Library/Containers/net.mtgto.inputmethod.macSKK/Data/Documents"
        $DRY_RUN_CMD /bin/mkdir -p "$container/Dictionaries" "$container/Settings"
        $DRY_RUN_CMD /usr/bin/install -m644 \
          "${pkgs.skkDictionaries.l}/share/skk/SKK-JISYO.L" \
          "$container/Dictionaries/SKK-JISYO.L"
        $DRY_RUN_CMD /usr/bin/install -m644 \
          "${./macskk/kana-rule.conf}" \
          "$container/Settings/kana-rule.conf"
      '';
    };
  };
}
