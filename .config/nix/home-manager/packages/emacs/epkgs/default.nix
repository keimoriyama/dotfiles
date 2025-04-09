{
  pkgs,
  sources,
}: epkgs: let
  lsp = import ./packages/lsp {inherit epkgs pkgs sources;};
  utils = import ./packages/utils {inherit epkgs pkgs sources;};
  git = import ./packages/git {inherit epkgs pkgs sources;};
  completion = import ./packages/completion {inherit epkgs pkgs sources;};
  org = import ./packages/org {inherit epkgs pkgs sources;};
  langs = import ./packages/langs {inherit epkgs pkgs sources;};
in
  lsp ++ utils ++ git ++ completion ++ org ++ langs
