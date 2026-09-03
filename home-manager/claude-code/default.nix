{
  config,
  pkgs,
  ...
}: let
  # dotfiles 側を実体にして symlink する。編集が switch なしで即反映される。
  link = path: config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home-manager/claude-code/${path}";
  cagePresets = link "cage-presets.yml";
in {
  home.file =
    {
      ".claude/settings.json" = {
        source = link "settings.json";
        force = true;
      };

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
