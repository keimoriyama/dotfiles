{pkgs}:
with pkgs; [
  # programming langauages
  nodejs_24
  typescript
  lua
  typst
  texliveBasic
  go
  ghc
  auctex
  perl
  zig_0_15

  # frameworks & tools
  deno
  rustup
  cargo-generate
  uv

  # language server protcols
  # pyright
  basedpyright
  ruff
  # ty
  pyrefly
  isort
  yaml-language-server
  lua-language-server
  stylua
  typescript-language-server
  haskell-language-server
  docker-language-server
  yaml-language-server
  haskell-language-server
  nixd
  texlab
  prettier
  cspell

  # formatter & linter
  alejandra
  tinymist
  typstyle
  fourmolu
  (textlint.withPackages [
    textlint-rule-preset-ja-technical-writing
    textlint-rule-preset-ja-spacing
    textlint-plugin-org
    textlint-plugin-latex2e
    textlint-rule-write-good
  ])
]
