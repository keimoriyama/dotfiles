{
  pkgs,
  org-babel,
  sources,
}: let
  tangle = org-babel.lib.tangleOrgBabel {languages = ["emacs-lisp"];};
  emacs = pkgs.emacs.overrideAttrs (old:
    pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
      postPatch =
        (old.postPatch or "")
        + ''
          substituteInPlace src/conf_post.h \
            --replace-fail '#if defined WINDOWSNT && !defined DEFER_MS_W32_H' '#include <stdalign.h>
          #include <stdbool.h>

          #if defined WINDOWSNT && !defined DEFER_MS_W32_H'
        '';
    });
  emacsPkgs = pkgs.emacsWithPackagesFromUsePackage {
    package = emacs;
    config = builtins.toFile "empty.el" "";
    extraEmacsPackages = import ./epkgs.nix {inherit pkgs sources;};
  };
in {
  programs.emacs = {
    enable = true;
    package = emacsPkgs;
  };
  home = {
    file = {
      ".emacs.d/init.el".text = tangle (builtins.readFile ./init.org);
      ".emacs.d/early-init.el".text = tangle (builtins.readFile ./early-init.org);
      ".emacs.d/misc/yasnippet.org".source = ./yasnippet.org;
    };
    # packages = with pkgs; [
    #   emacs-lsp-booster
    # ];
  };
}
