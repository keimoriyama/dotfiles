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
    packages.nskk
    rainbow-delimiters
    # hydra
    # pretty-hydra
    major-mode-hydra
    tempel
    quickrun
    avy-zap
    ellama
    yasnippet
    consult-yasnippet
    yasnippet-capf
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
    # moody
    # minions
    neotree
    tree-sitter
    centered-cursor-mode
    pdf-tools
    # packages.kuro
    packages.arto

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
    # rustowl
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
    # lsp-bridge
    lsp-mode
    lsp-ui
    consult-lsp
    # dap-mode
    flycheck
    flycheck-posframe
    flycheck-rust
    highlight-indent-guides
    lsp-latex
    lsp-pyright
    lsp-haskell
    # lsp-nix

    # org
    org-superstar
    ox-gfm
    ox-typst
    org-modern
    org-pomodoro
    svg-tag-mode
    ox-hugo
    org-roam
    org-roam-ui
    consult-org-roam
    org-super-agenda
    org-appear
    ob-rust
    packages.hide-lines
  ]
