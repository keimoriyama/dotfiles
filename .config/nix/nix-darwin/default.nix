{pkgs, ...}: {
  nix = {
    optimise.automatic = true;
    # enable = false;
    settings = {
      experimental-features = "nix-command flakes";
      max-jobs = 8;
    };
  };
  # システムの設定
  system = {
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
      remapCapsLockToControl = true;
    };
  };

  # homebrewの設定
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      # !! 注意 !!
      cleanup = "uninstall";
    };
    taps = [
      "wtsnjp/tex2img"
    ];
    casks = [
      # ここにGUIアプリの記述
      "aquaskk"
      "skim"
      "discord"
      # "slack"
      "font-hack-nerd-font"
      # "spotify"
      # "karabiner-elements"
      "zoom"
      "mactex"
      "notion"
      # "wezterm"
      # "docker"
      # "google-chrome"
      # "ollama"
    ];
  };
}
