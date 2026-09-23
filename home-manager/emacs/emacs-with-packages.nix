# 本家 Emacs の設定と neomacs の設定の両方から参照するので、単独のファイルに
# 切り出してある。どちらから呼んでも同じ derivation になる。
{
  pkgs,
  sources,
}:
pkgs.emacsWithPackagesFromUsePackage {
  package = pkgs.emacs;
  config = builtins.toFile "empty.el" "";
  extraEmacsPackages = import ./epkgs.nix {inherit pkgs sources;};
}
