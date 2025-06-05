{
  epkgs,
  pkgs,
}: let
  packages = pkgs.callPackage ./packages.nix {inherit epkgs;};
in
  with epkgs; [
    olivetti
    org-superstar
    ox-gfm
    org-modern
    org-pomodoro
  ]
