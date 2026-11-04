{pkgs, ...}: {
  programs.zed-editor.enable = pkgs.stdenv.hostPlatform.isDarwin;
}
