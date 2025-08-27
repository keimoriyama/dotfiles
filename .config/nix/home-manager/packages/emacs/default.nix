{
  lib,
  pkgs,
}: let
  override = final: prev: {
    dap-mode = prev.melpaPackages.dap-mode.overrideAttrs (old: {
      preBuild = null;
    });
  };
in {
  emacs-stable = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    # package = pkgs.emacs;
    # package = pkgs.emacs-unstable.overrideAttrs (old: {
    #   # buildInputs = old.buildInputs;
    #   configureFlags = old.configureFlags ++ ["--with-xwidgets"];
    # });
    extraEmacsPackages = import ./epkgs {inherit pkgs;};
    override = override;
  };
}
