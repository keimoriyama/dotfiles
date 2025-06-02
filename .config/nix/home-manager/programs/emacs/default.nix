{
  pkgs,
  org-babel,
  emacsPkg,
}: let
  tangle = org-babel.lib.tangleOrgBabel {languages = ["elisp"];};
in {
  programs.emacs = {
    enable = true;
    package = emacsPkg;
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
    # packages = with pkgs; [
    #   (emacs-lsp-booster.overrideAttrs (
    #     old: {
    #       doCheck = false;
    #       nativeCheckInputs = [];
    #     }
    #   ))
    # ];
  };
}
