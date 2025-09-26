{
  pkgs,
  config,
  org-babel,
  emacsPkg,
  nodePkgs,
  sources,
}: let
  emacs = import ./emacs {inherit pkgs org-babel emacsPkg;};
  fish = import ./fish {inherit pkgs;};
  wezterm = import ./wezterm {inherit pkgs;};
  copilot-language-server = import ./copilot-language-server {inherit pkgs nodePkgs;};
  neovim = import ./neovim {inherit pkgs sources config;};
  git = import ./git;
  nushell = import ./nushell {inherit pkgs;};
in [
  emacs
  fish
  wezterm
  copilot-language-server
  neovim
  git
  nushell
]
