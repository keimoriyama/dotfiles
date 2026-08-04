{
  emacs-overlay,
  # neovim-nightly-overlay,
  brew-nix,
}: [
  (import emacs-overlay)
  # (import neovim-nightly-overlay)
  brew-nix.overlays.default
  # mailutils は movemail (ローカルメールスプール読み込み) 用の依存だが
  # aarch64-darwin でリンクに失敗する。emacsPackages も final.emacs から
  # 派生するので、個別に override せずここで一括して落とす。
  (_final: prev: {
    emacs = prev.emacs.override {withMailutils = false;};
  })
]
