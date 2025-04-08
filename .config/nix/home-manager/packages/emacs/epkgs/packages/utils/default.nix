{
  epkgs,
  pkgs,
  sources,
}: let
  packages = pkgs.callPackage ./packages.nix {inherit sources epkgs;};
in
  with epkgs; [
  dash
  f
  nerd-icons-completion
  solarized-theme
  volatile-highlights
  projectile
  expand-region
  undo-tree
  # hl-line
  free-keys
  puni
  autorevert
  simple
  spaceline
  startup
  mistty
  which-key
  exec-path-from-shell
  ]
