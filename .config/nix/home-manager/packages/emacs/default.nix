{
  lib,
  pkgs,
  sources,
}: let
    dap-mode = prev.melpaPackages.dap-mode.overrideAttrs (old: {
  override = final: prev: {
      preBuild = null;
    });
  };
in {
  emacs-stable = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs;
    extraEmacsPackages = import ./epkgs {inherit pkgs sources;};
    override = override;
  };
}
