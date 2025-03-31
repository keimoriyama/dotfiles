{
  pkgs,
  org-babel,
  emacsPkg,
}: let
  emacs = import ./emacs {inherit pkgs org-babel emacsPkg;};
in [
  emacs
]
