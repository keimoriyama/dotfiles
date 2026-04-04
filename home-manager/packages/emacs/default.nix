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
    env = {
      WEBKIT_DISABLE_COMPOSITING_MODE = "1";
      NATIVE_COMP_JOBS = "0";
    };
  };
in {
  emacs-stable = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs.overrideAttrs (old:
      parallelBuildAttrs
      // {
        buildInputs =
          old.buildInputs
          ++ lib.optionals pkgs.stdenv.isDarwin [pkgs.apple-sdk];
        configureFlags =
          old.configureFlags
          ++ lib.optional pkgs.stdenv.isDarwin "--with-xwidgets";
        env = (old.env or {}) // parallelBuildAttrs.env;
      });
    extraEmacsPackages = import ./epkgs.nix {inherit pkgs sources;};
    override = override;
  };
}
