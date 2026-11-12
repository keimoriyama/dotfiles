{pkgs, ...}: {
  home.packages = with pkgs; [
    tree-sitter
    curl
    rsync
    peco
    ghq
    gh
    ripgrep
    ghostscript
    pandoc
    sl
    ispell
    tdf
    nvfetcher
    hugo
    fzf
    wget
    tree
    udev-gothic
    nerd-fonts.symbols-only
  ];
}
