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
    all-the-icons
    nerd-icons
    nerd-icons-completion
    catppuccin-theme
    volatile-highlights
    packages.projectile
    expand-region
    undo-tree
    free-keys
    bufferlo
    puni
    which-key
    exec-path-from-shell
    ddskk
    ddskk-posframe
    packages.nskk
    rainbow-delimiters
    major-mode-hydra
    avy-zap
    yasnippet
    consult-yasnippet
    oj
    dashboard
    htmlize
    gcmh
    beacon
    symbol-overlay
    color-identifiers-mode
    packages.dmacro
    packages.instant-maximized-window
    packages.nano-modeline
    packages.arto

    # completion
    corfu
    nerd-icons-corfu
    kind-icon
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
    auctex
    packages.yatex
    packages.typst-ts-mode
    python-mode
    pet
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
    fish-mode
    haskell-mode
    lua-mode
    web-mode

    # ai
    copilot
    ellama
    llm
    copilot-chat
    agent-shell

    # lsp
    packages.eglot-x
    eglot-booster
    flycheck
    flycheck-posframe
    highlight-indent-guides
    flycheck-rust

    # org
    org-superstar
    ox-gfm
    ox-typst
    org-pomodoro
    svg-tag-mode
    ox-hugo
    org-roam
    org-roam-ui
    org-super-agenda
    org-appear
    org-journal
    ob-rust
    packages.hide-lines
  ]
