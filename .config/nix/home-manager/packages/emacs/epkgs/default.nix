{pkgs}: epkgs: let
  lsp = import ./packages/lsp {inherit epkgs pkgs;};
  utils = import ./packages/utils {inherit epkgs pkgs;};
  git = import ./packages/git {inherit epkgs pkgs;};
  completion = import ./packages/completion {inherit epkgs pkgs;};
  org = import ./packages/org {inherit epkgs pkgs;};
  langs = import ./packages/langs {inherit epkgs pkgs;};
in
  lsp ++ utils ++ git ++ completion ++ org ++ langs
