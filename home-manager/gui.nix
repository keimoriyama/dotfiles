{
  pkgs,
  lib,
  isWork ? false,
  ...
}: {
  # 業務用マシンでは GUI アプリは会社の配布物を使うため home-manager では入れない。
  home.packages = lib.optionals (pkgs.stdenv.hostPlatform.isDarwin && !isWork) (with pkgs; [
    slack
    google-chrome
  ]);
}
