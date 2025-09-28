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
    mistty
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
    doom-modeline
    oj
    dashboard
    htmlize
    ox-hugo
    org-roam
    org-super-agenda
    gcmh
    beacon

    # completion
    corfu
    nerd-icons-corfu
    cape
    vertico
    marginalia
    avy
    consult
    embark
    embark-consult
    affe
    orderless

    # git
    git-gutter
    magit

    # langs
    python-mode
    pet
    lsp-pyright
    ruff-format
    rust-mode
    cargo
    yaml-mode
    nix-mode
    dockerfile-mode
    markdown-mode
    lsp-latex
    auctex
    packages.yatex
    yasnippet
    consult-yasnippet
    packages.copilot
    web-mode

    # lsp
    lsp-mode
    lsp-ui
    # dap-mode
    flycheck
    highlight-indent-guides

    # org
    org-superstar
    ox-gfm
    org-modern
    org-pomodoro
  ]
