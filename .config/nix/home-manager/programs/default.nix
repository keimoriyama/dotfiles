{
  pkgs,
  org-babel,
  emacsPkg,
  nodePkgs,
}: let
  emacs = import ./emacs {inherit pkgs org-babel emacsPkg;};
  fish = import ./fish {inherit pkgs;};
  wezterm = import ./wezterm {inherit pkgs;};
  copilot-language-server = import ./copilot-language-server {inherit pkgs nodePkgs;};
in [
  emacs
  fish
  wezterm
  copilot-language-server
]
