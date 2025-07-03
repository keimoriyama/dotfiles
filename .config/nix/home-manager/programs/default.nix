{
  pkgs,
  org-babel,
  emacsPkg,
}: let
  emacs = import ./emacs {inherit pkgs org-babel emacsPkg;};
  fish = import ./fish {inherit pkgs;};
  wezterm = import ./wezterm {inherit pkgs;};
in [
  # emacs
  fish
  wezterm
]
