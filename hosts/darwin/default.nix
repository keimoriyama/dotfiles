{pkgs, ...}: let
  aerospace = import ../../nix-darwin/aerospace {inherit pkgs;};
  userShell = builtins.toPath "/etc/profiles/per-user/kei/bin/fish";
in {
  imports = [
    aerospace
  ];

  nix.package = pkgs.nix;

  environment.systemPackages = [
    # pkgs.zoom-us
    # pkgs.macskk
  ];
  environment.shells = [userShell];
  environment.etc."shells".knownSha256Hashes = [
    "9d5aa72f807091b481820d12e693093293ba33c73854909ad7b0fb192c2db193"
    "135896d22e1bb2cc94d76895b06ea185ec470551648ce841eb7adea623026970"
  ];

  nixpkgs.config.allowUnfree = true;

  nix = {
    optimise.automatic = true;
    settings = {
      experimental-features = "nix-command flakes";
      max-jobs = 8;
    };
  };

  programs.fish.enable = true;

  users.users.kei = {
    name = "kei";
    home = "/Users/kei";
    shell = userShell;
  };

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

  # homebrew = {
  #   enable = true;
  #   onActivation = {
  #     autoUpdate = true;
  #     upgrade = true;
  #     cleanup = "uninstall";
  #   };
  #   casks = [
  #     # "macskk"
  #     # "zoom"
  #   ];
  # };
}
