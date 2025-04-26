{
  epkgs,
  pkgs,
  sources,
}: let
  packages = pkgs.callPackage ./packages.nix {inherit sources epkgs;};
in
  with epkgs; [
    python-mode
    pet
    lsp-pyright
    yaml-mode
    nix-mode
    dockerfile-mode
    yatex
    yasnippet
    # flyspell
    # consult-reftex
    # reftex
  ]
