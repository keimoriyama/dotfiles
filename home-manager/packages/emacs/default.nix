{
  lib,
  pkgs,
  sources,
}: let
  emacs = pkgs.emacs.overrideAttrs (old:
    lib.optionalAttrs pkgs.stdenv.isDarwin {
      postPatch =
        (old.postPatch or "")
        + ''
          substituteInPlace src/conf_post.h \
            --replace-fail '#if defined WINDOWSNT && !defined DEFER_MS_W32_H' '#include <stdalign.h>
          #include <stdbool.h>

          #if defined WINDOWSNT && !defined DEFER_MS_W32_H'
        '';
    });
in {
  emacs-stable = pkgs.emacsWithPackagesFromUsePackage {
    package = emacs;
    config = builtins.toFile "empty.el" "";
    extraEmacsPackages = import ./epkgs.nix {inherit pkgs sources;};
  };
}
