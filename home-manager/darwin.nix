{
  pkgs,
  lib,
  isWork ? false,
  ...
}: {
  home.packages = lib.optionals pkgs.stdenv.hostPlatform.isDarwin (with pkgs.brewCasks;
    [
      skim
      # The claude desktop cask ships a `bin/claude` wrapper that collides with the
      # claude-code CLI's `bin/claude`. Lower its priority so the CLI wins the bin/
      # while the desktop .app bundle (a non-conflicting path) is still installed.
      (lib.lowPrio claude)
    ]
    # 業務用マシンでは Zoom は会社の配布物を使う。
    ++ lib.optional (!isWork) zoom
    # 業務用マシンでは codex 系と同様に OpenAI のクライアントを入れない。
    ++ lib.optional (!isWork) chatgpt);
}
