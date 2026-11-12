{
  pkgs,
  lib,
  isWork ? false,
  ...
}: {
  home.packages = lib.optionals pkgs.stdenv.hostPlatform.isDarwin (with pkgs.brewCasks;
    [
      skim
    ]
    # 業務用マシンでは Zoom は会社の配布物を使う。
    ++ lib.optional (!isWork) zoom);
}
