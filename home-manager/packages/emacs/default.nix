{
  lib,
  pkgs,
  sources,
}: let
  override = _: prev: {
    lsp-mode = prev.melpaPackages.lsp-mode.overrideAttrs (old: {
      env =
        (old.env or {})
        // {
          LSP_USE_PLISTS = "1";
        };
    });
  };
  parallelBuildAttrs = {
    enableParallelBuilding = true;
    env = {};
  };
in {
  emacs-stable = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs.overrideAttrs (old:
      parallelBuildAttrs
      // {
        buildInputs =
          old.buildInputs
          ++ lib.optional pkgs.stdenv.isDarwin [pkgs.apple-sdk]
          ++ lib.optionals pkgs.stdenv.isLinux (with pkgs; [
            gtk3
            webkitgtk
            glib-networking
          ]);
        configureFlags = old.configureFlags ++ ["--with-xwidgets"];
        env = (old.env or {}) // parallelBuildAttrs.env;
      });
    extraEmacsPackages = import ./epkgs.nix {inherit pkgs sources;};
    override = override;
  };
}
