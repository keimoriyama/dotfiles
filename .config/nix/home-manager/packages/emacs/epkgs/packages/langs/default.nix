{
  epkgs,
  pkgs,
}: let
  packages = pkgs.callPackage ./packages.nix {inherit epkgs;};
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
    # yatex
    yasnippet
    consult-yasnippet
    # flyspell
    # consult-reftex
    # reftex
  ]
