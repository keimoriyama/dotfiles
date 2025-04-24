{
  epkgs,
  pkgs,
  sources,
}: let
  packages = pkgs.callPackage ./packages.nix {inherit sources epkgs;};
in
  with epkgs; [
    olivetti
    org-superstar
    ox-gfm
    org-pomodoro
  ]
