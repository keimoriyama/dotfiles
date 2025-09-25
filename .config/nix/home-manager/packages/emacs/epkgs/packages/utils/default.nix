{
  epkgs,
  pkgs,
}: let
  packages = pkgs.callPackage ./packages.nix {inherit epkgs;};
in
  with epkgs; [
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
  ]
