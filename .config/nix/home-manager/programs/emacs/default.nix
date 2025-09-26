{
  pkgs,
  org-babel,
  emacsPkgs,
  sources
}: let
  tangle = org-babel.lib.tangleOrgBabel {languages = ["elisp"];};
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
    packages = with pkgs; [
      emacs-lsp-booster
    ];
  };
}
