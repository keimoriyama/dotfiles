{
  pkgs,
  username,
  ...
}: {
  environment.systemPackages = [
    # pkgs.zoom-us
    # pkgs.macskk
  ];

  nixpkgs.config.allowUnfree = true;

  # Determinate Nix manages the Nix installation via its own daemon, which
  # conflicts with nix-darwin's native management. Disabling it lets the two
  # coexist. With nix.enable = false, nix-darwin refuses to manage any other
  # nix.* option (package/settings/optimise), so those are configured through
  # Determinate instead (e.g. /etc/nix/nix.custom.conf); it enables flakes and
  # nix-command by default.
  nix.enable = false;

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
}
