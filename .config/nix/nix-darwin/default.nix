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
      NSGlobalDomain.AppleShowAllExtensions = true;
      finder = {
        AppleShowAllFiles = false;
        AppleShowAllExtensions = true;
      };
      dock = {
        autohide = true;
        show-recents = false;
        orientation = "left";
      };
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
      "arc"
      "sublime-text"
      "aquaskk"
      "skim"
      "discord"
      "slack"
      "font-hack-nerd-font"
      "spotify"
      "wezterm"
      "karabiner-elements"
      "zoom"
      "mactex"
      "notion"
      "notion-calendar"
    ];
  };
}
