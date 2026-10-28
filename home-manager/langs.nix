{
  pkgs,
  lib,
  config,
  ...
}: {
  # Adobe Fonts (Creative Cloud) が同期するフォントを typst / tinymist から使えるようにする
  home.sessionVariables = lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
    TYPST_FONT_PATHS = lib.concatStringsSep ":" [
      "${config.home.homeDirectory}/Library/Application Support/Adobe/.User Owned Fonts"
      "${config.home.homeDirectory}/Library/Application Support/Adobe/CoreSync/plugins/livetype/.r"
    ];
  };

  home.packages = with pkgs; [
    # programming languages
    typst
    texliveFull
    ghc
    auctex
    perl

    # frameworks & tools
    rustup
    uv

    # language servers
    ruff
    ty
    isort
    yaml-language-server
    lua-language-server
    stylua
    typescript-language-server
    haskell-language-server
    docker-language-server
    nixd
    prettier
    cspell

    # formatter & linter
    alejandra
    tinymist
    typstyle
    fourmolu
    shfmt
    (textlint.withPackages [
      textlint-rule-preset-ja-technical-writing
      textlint-rule-preset-ja-spacing
      textlint-plugin-org
      textlint-plugin-latex2e
      textlint-rule-write-good
    ])
  ];
}
