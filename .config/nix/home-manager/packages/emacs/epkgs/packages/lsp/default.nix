{
  epkgs,
  pkgs,
}: let
  packages = pkgs.callPackage ./packages.nix {inherit epkgs;};
in
  with epkgs; [
    # lsp
    lsp-mode
    lsp-ui
    # dap-mode
    flycheck
    highlight-indent-guides
  ]
