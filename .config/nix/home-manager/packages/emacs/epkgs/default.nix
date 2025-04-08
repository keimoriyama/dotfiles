{
  pkgs,
  sources,
}: epkgs: let
  lsp = import ./packages/lsp {inherit epkgs pkgs sources;};
  utils = import ./packages/utils {inherit epkgs pkgs sources;};
  git = import ./packages/git{inherit epkgs pkgs sources;};
  completion= import ./packages/completion {inherit epkgs pkgs sources;};
in
  lsp ++ utils ++ git ++ completion
