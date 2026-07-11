{
  pkgs,
  org-babel,
  sources,
}: let
  # Tangle the emacs-lisp blocks out of an .org config into a plain init string.
  tangle = org-babel.lib.tangleOrgBabel {languages = ["emacs-lisp"];};
  tangleOrg = org: tangle (builtins.readFile org);

  # On Darwin the emacs build needs stdalign.h/stdbool.h pulled in before the
  # conf_post.h platform guards, otherwise `alignas`/`bool` are undefined.
  # emacs = pkgs.emacs.overrideAttrs (old:
  #   pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
  #     postPatch =
  #       (old.postPatch or "")
  #       + ''
  #         substituteInPlace src/conf_post.h \
  #           --replace-fail '#if defined WINDOWSNT && !defined DEFER_MS_W32_H' '#include <stdalign.h>
  #         #include <stdbool.h>
  #
  #         #if defined WINDOWSNT && !defined DEFER_MS_W32_H'
  #       '';
  #   });

  emacsPkgs = pkgs.emacsWithPackagesFromUsePackage {
    package = pkgs.emacs;
    config = builtins.toFile "empty.el" "";
    extraEmacsPackages = import ./epkgs.nix {inherit pkgs sources;};
  };

  buildGhostelBackend = import ./ghostel-backend.nix {inherit pkgs sources;};
in {
  programs.emacs = {
    enable = true;
    package = emacsPkgs;
  };
  home = {
    file = {
      ".emacs.d/init.el".text = tangleOrg ./init.org;
      ".emacs.d/early-init.el".text = tangleOrg ./early-init.org;
      ".emacs.d/misc/yasnippet.org".source = ./yasnippet.org;
    };
    packages = with pkgs; [
      emacs-lsp-booster
      buildGhostelBackend
    ];
  };
}
