{pkgs}:
with pkgs; [
  # programming langauages
  nodejs_24
  typescript
  lua
  typst
  texliveFull
  go
  ghc
  auctex
  perl

  # frameworks & tools
  deno
  rustup
  cargo-generate
  uv

  # language server protcols
  basedpyright
  ruff
  ty
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

  # formatter & linter
  alejandra
  tinymist
  typstyle
  fourmolu
  (textlint.withPackages [
    textlint-rule-preset-ja-technical-writing
    textlint-plugin-org
    textlint-plugin-latex2e
  ])
]
