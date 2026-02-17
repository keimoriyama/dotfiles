{
  lib,
  pkgs,
  sources,
}: let
  override = final: prev: {
    dap-mode = prev.melpaPackages.dap-mode.overrideAttrs (old: {
      preBuild = null;
    });
  };
  parallelBuildAttrs = {
    enableParallelBuilding = true;
    env = {
      NATIVE_COMP_JOBS = "0";
    };
  };
in {
  emacs-stable = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs.overrideAttrs (old:
      parallelBuildAttrs
      // {
        buildInputs = old.buildInputs ++ lib.optional pkgs.stdenv.isDarwin [pkgs.apple-sdk];
        configureFlags = old.configureFlags ++ ["--with-xwidgets" "--with-dbus"];
        env = (old.env or {}) // parallelBuildAttrs.env;
      });
    extraEmacsPackages = import ./epkgs.nix {inherit pkgs sources;};
    override = override;
  };
}
