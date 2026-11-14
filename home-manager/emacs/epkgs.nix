{
  pkgs,
  sources,
}: epkgs: let
  packages = pkgs.callPackage ./packages.nix {inherit pkgs epkgs sources;};
in
  with epkgs; [
    # utils
    olivetti
    dash
    f
    nerd-icons
    nerd-icons-completion
    catppuccin-theme
    volatile-highlights
    packages.projectile
    undo-tree
    bufferlo
    puni
    which-key
    exec-path-from-shell
    ddskk
    ddskk-posframe
    rainbow-delimiters
    major-mode-hydra
    yasnippet
    consult-yasnippet
    dashboard
    htmlize
    gcmh
    beacon
    color-identifiers-mode
    packages.nano-modeline
    packages.arto
    rsync-mode

    # completion
    corfu
    kind-icon
    cape
    vertico
    marginalia
    consult
    embark
    embark-consult
    orderless

    # git
    ghq
    consult-gh
    consult-ghq
    git-gutter
    magit

    # langs
    packages.yatex
    packages.typst-ts-mode
    nix-mode
    rust-mode
    cargo
    reformatter
    csv-mode
    dotenv-mode
    git-modes
    ssh-config-mode

    fish-mode
    haskell-mode
    web-mode

    # ai
    agent-shell
    packages.agent-shell-notifications
    packages.agent-shell-attention
    packages.agent-shell-tramp
    packages.agent-shell-dashboard

    # lsp
    packages.eglot-x
    eglot-booster
    flycheck
    highlight-indent-guides
    flycheck-rust

    # org
    org-superstar
    ox-gfm
    ox-typst
    svg-tag-mode
    ox-hugo
    org-roam
    org-super-agenda
    org-appear
    org-journal
    ob-rust
  ]
