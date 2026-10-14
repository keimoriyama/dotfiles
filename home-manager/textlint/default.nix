{config, ...}: {
  # textlint は org / latex2e をプラグイン経由で解析するため、
  # プラグインを有効にした設定をホームに置く。
  home.file.".textlintrc.json".source =
    config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/ghq/github.com/keimoriyama/dotfiles/home-manager/textlint/textlintrc.json";
}
