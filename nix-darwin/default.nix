{pkgs, ...}: {
  # Nixデーモンの自動アップグレードを有効化
  nix.package = pkgs.nix;
  environment.systemPackages = [
    pkgs.zoom-us
    pkgs.macskk
  ];
  # 非自由パッケージを許可
  nixpkgs.config.allowUnfree = true;

  nix = {
    optimise.automatic = true;
    # enable = false;
    settings = {
      experimental-features = "nix-command flakes";
      # sandbox = true;
      max-jobs = 8;
    };
  };
  # システムの設定
  system = {
    primaryUser = "kei";
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
        orientation = "left";
        static-only = true;
        tilesize = 32;
      };
    };
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };

  # homebrewの設定
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      # !! 注意 !!
      upgrade = true;
      cleanup = "uninstall";
    };
    # taps = [
    #   "wtsnjp/tex2img"
    # ];
    casks = [
      # ここにGUIアプリの記述
      "macskk"
      # "zoom"
    ];
  };
}
