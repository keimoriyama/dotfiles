{
  pkgs,
  username,
  ...
}: {
  nix.package = pkgs.nix;

  environment.systemPackages = [
    # pkgs.zoom-us
    # pkgs.macskk
  ];

  nixpkgs.config.allowUnfree = true;

  nix = {
    optimise.automatic = true;
    settings = {
      experimental-features = "nix-command flakes";
      max-jobs = 8;
    };
  };

  system = {
    primaryUser = username;
    stateVersion = 6;
    defaults = {
      NSGlobalDomain = {
        AppleShowAllExtensions = true;
        "com.apple.sound.beep.feedback" = 0;
        "com.apple.sound.beep.volume" = 0.0;
      };
      finder = {
        AppleShowAllFiles = false;
        AppleShowAllExtensions = true;
      };
      dock = {
        autohide = true;
        show-recents = false;
        orientation = "bottom";
        static-only = true;
        tilesize = 32;
      };
    };
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };

  # `system.keyboard.remapCapsLockToControl` は hidutil を switch 時と起動時に
  # 一度だけ実行する。hidutil のリマップはコマンド実行時に接続済みの HID
  # デバイスにしか適用されず、あとから接続・再列挙されたキーボード
  # (外付け接続 / スリープ復帰での Bluetooth 再接続 / USB 再列挙) はデフォルトに
  # 戻ってしまう。これが「Caps Lock → Control がたまに無効になる」原因。
  # イベント駆動 (IOKit マッチング) は発火条件が環境依存で検証しづらいため、
  # 同じマッピングを定期的に再適用する daemon で確実に取りこぼしを塞ぐ。
  launchd.daemons.remap-caps-lock-to-control = {
    serviceConfig = {
      ProgramArguments = [
        "/usr/bin/hidutil"
        "property"
        "--set"
        # Src 0x700000039 = caps_lock, Dst 0x7000000E0 = left_control
        ''{"UserKeyMapping":[{"HIDKeyboardModifierMappingSrc":0x700000039,"HIDKeyboardModifierMappingDst":0x7000000E0}]}''
      ];
      RunAtLoad = true;
      StartInterval = 10;
      # 適用は一瞬で終わるので出力は捨てる。
      StandardOutPath = "/dev/null";
      StandardErrorPath = "/dev/null";
    };
  };
}
