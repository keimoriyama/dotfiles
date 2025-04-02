{
  pkgs,
  org-babel,
  emacsPkg,
}: let
  emacs = import ./emacs {inherit pkgs org-babel emacsPkg;};
  nvim = import ./nvim {inherit pkgs;};
in [
  emacs
  nvim
]
