{
  config,
  pkgs,
  isWork ? false,
  ...
}: let
  # dotfiles 側を実体にして symlink する。編集が switch なしで即反映される。
  link = path: config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home-manager/claude-code/${path}";
  cagePresets = link "cage-presets.yml";

  # statusline の出し分け。仕事用は Enterprise 契約でレート制限が stdin に来ず、
  # OAuth 経由の Org バー (今月のクレジット消費) だけが意味を持つ。
  # 個人用は Pro/Max で 5h/7d の使用率が stdin に来るので、OAuth 取得は止めて
  # Org バーも隠す。
  statuslineArgs =
    if isWork
    then ""
    else " --hide org";
  statuslineScript = pkgs.writeShellScript "claude-statusline" ''
    ${pkgs.lib.optionalString (!isWork) "export CLAUDE_USAGE_LINE_NO_FETCH=1"}
    exec claude-usage-line${statuslineArgs}
  '';
in {
  home.file =
    {
      ".claude/settings.json" = {
        source = link "settings.json";
        force = true;
      };

      # settings.json からはこのスクリプトを呼ぶだけにして、
      # 仕事用と個人用の差分をここに閉じ込める。
      ".claude/statusline.sh".source = statuslineScript;

      # PreToolUse hook が読む。危険な操作をブロックしつつ代替手段を伝える。
      ".config/guard-and-guide/rules.toml".source = link "guard-and-guide-rules.toml";

      # cage は XDG_CONFIG_HOME を見るが、未設定だと macOS では
      # ~/Library/Application Support を、Linux では ~/.config を見る。
      # どちらでも拾えるよう両方に置く。
      ".config/cage/presets.yml".source = cagePresets;
    }
    // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
      "Library/Application Support/cage/presets.yml".source = cagePresets;
    };
}
