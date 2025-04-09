{
  epkgs,
  pkgs,
  sources,
}: let
  packages = pkgs.callPackage ./packages.nix {inherit sources epkgs;};
in
  with epkgs; [
    corfu
    nerd-icons-corfu
    cape
    vertico
    marginalia
    avy
    consult
    embark
    embark-consult
    affe
    orderless
  ]
