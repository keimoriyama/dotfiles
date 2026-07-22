{
  pkgs,
  org-babel,
  sources,
}: let
  # Tangle the emacs-lisp blocks out of an .org config into a plain init string.
  tangle = org-babel.lib.tangleOrgBabel {languages = ["emacs-lisp"];};
  tangleOrg = org: tangle (builtins.readFile org);

  emacsPkgs = pkgs.emacsWithPackagesFromUsePackage {
    package = pkgs.emacs;
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
      ".emacs.d/init.el".text = tangleOrg ./init.org;
      ".emacs.d/early-init.el".text = tangleOrg ./early-init.org;
      ".emacs.d/misc/yasnippet.org".source = ./yasnippet.org;
    };
    packages = with pkgs; [
      emacs-lsp-booster
    ];
  };
}
