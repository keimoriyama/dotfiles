{
  epkgs,
  pkgs,
  sources,
}: let
  packages = pkgs.callPackage ./packages.nix {inherit epkgs sources;};
in
  with epkgs; [
    python-mode
    pet
    lsp-pyright
    ruff-format
    rust-mode
    cargo
    yaml-mode
    nix-mode
    dockerfile-mode
    markdown-mode
    lsp-latex
    auctex
    packages.yatex
    yasnippet
    consult-yasnippet
    packages.copilot
  ]
