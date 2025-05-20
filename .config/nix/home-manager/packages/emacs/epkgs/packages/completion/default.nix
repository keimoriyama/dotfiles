{
  epkgs,
  pkgs,
}: let
  packages = pkgs.callPackage ./packages.nix {inherit epkgs;};
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
