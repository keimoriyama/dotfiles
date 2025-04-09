{
  epkgs,
  pkgs,
  sources,
}: let
  packages = pkgs.callPackage ./packages.nix {inherit sources epkgs;};
in
  with epkgs; [
    lsp-bridge
    lsp-mode
    lsp-ui
    # dap-mode
    flycheck
    highlight-indent-guides
  ]
