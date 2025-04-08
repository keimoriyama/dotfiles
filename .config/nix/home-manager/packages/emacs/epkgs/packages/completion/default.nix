{
  epkgs,
  pkgs,
  sources,
}: let
  packages = pkgs.callPackage ./packages.nix {inherit sources epkgs;};
in
  with epkgs; [
  corfu
  cape
  vertico
  marginalia
  avy
  consult
  embark
  affe
  orderless
  ]
