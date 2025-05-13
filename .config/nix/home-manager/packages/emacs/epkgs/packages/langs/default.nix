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
    ruff-format
    yaml-mode
    nix-mode
    dockerfile-mode
    yatex
    yasnippet
	consult-yasnippet
    # flyspell
    # consult-reftex
    # reftex
  ]
