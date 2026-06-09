{
  lib,
  pkgs,
  sources,
}: let
in {
  emacs-stable = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    extraEmacsPackages = import ./epkgs.nix {inherit pkgs sources;};
  };
}
