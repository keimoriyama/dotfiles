{pkgs, ...}: {

  nix = {
    optimise.automatic = true;
	# enable = false;
    settings = {
      experimental-features = "nix-command flakes";
      max-jobs = 8;
    };
  };

  system = {
  stateVersion = 6;
    defaults = {
      NSGlobalDomain.AppleShowAllExtensions = true;
      finder = {
        AppleShowAllFiles = true;
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
      # cleanup = "uninstall";
    };
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
    ];
  };
}
