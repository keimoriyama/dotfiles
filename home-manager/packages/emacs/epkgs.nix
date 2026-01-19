{
  pkgs,
  sources,
}: epkgs: let
  packages = pkgs.callPackage ./packages.nix {inherit epkgs sources;};
in
  with epkgs; [
    # utils
    olivetti
    dash
    f
    all-the-icons
    nerd-icons
    nerd-icons-completion
    solarized-theme
    catppuccin-theme
    volatile-highlights
    projectile
    expand-region
    undo-tree
    free-keys
    bufferlo
    puni
    spaceline
    which-key
    exec-path-from-shell
    ddskk
    ddskk-posframe
    rainbow-delimiters
    # hydra
    # pretty-hydra
    major-mode-hydra
    tempel
    quickrun
    avy-zap
    ellama
    # doom-modeline
    yasnippet
    consult-yasnippet
    oj
    dashboard
    htmlize
    ox-hugo
    org-roam
    org-super-agenda
    gcmh
    beacon
    symbol-overlay
    color-identifiers-mode
    packages.dmacro
    packages.instant-maximized-window
    packages.nano-modeline
    moody
    minions
    neotree
    tree-sitter

    # completion
    corfu
    nerd-icons-corfu
    cape
    vertico
    marginalia
    avy
    consult
    consult-flycheck
    consult-eglot
    consult-eglot-embark
    embark
    embark-consult
    affe
    orderless

    # git
    git-gutter
    magit

    # langs
    auctex
    packages.yatex
    copilot
    packages.typst-ts-mode
    python-mode
    pet
    # lsp-pyright
    nix-mode
    reformatter
    csv-mode
    docker-compose-mode
    dockerfile-mode
    dotenv-mode
    git-modes
    json-mode
    rust-mode
    cargo
    ssh-config-mode
    toml-mode
    yaml-mode
    python-pytest

    # lsp
    dap-mode
    flycheck
    flycheck-posframe
    highlight-indent-guides
    eglot-booster

    # org
    org-superstar
    ox-gfm
    ox-typst
    org-modern
    org-pomodoro
  ]
