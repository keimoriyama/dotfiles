{
  pkgs,
  org-babel,
  emacsPkg,
}: let
  tangle = org-babel.lib.tangleOrgBabel {languages = ["emacs-lisp"];};
in {
  programs.emacs = {
    enable = true;
    package = emacsPkg;
  };
  home = {
    file = {
      ".emacs.d/init.el".text = tangle (builtins.readFile ./init.org);
    };
    packages = with pkgs; [
      emacs-lsp-booster
    ];
  };
}
