{
  config,
  pkgs,
  lib,
  modulesPath,
  ...
}: {
  imports = [
    "${modulesPath}/profiles/minimal.nix"
    ./configuration.nix
  ];

  wsl = {
    enable = true;
    defaultUser = "kei";
    startMenuLaunchers = true;

    # Enable native Docker support
    docker-native.enable = false;

    # Enable integration with Windows
    wslConf = {
      automount.root = "/mnt";
      network.generateHosts = false;
      network.generateResolvConf = false;
    };
  };

  # Set your time zone
  time.timeZone = "America/Los_Angeles";

  # Networking
  networking = {
    hostName = "nixos-wsl";
    nameservers = ["8.8.8.8" "8.8.4.4"];
  };

  # Enable nix flakes
  nix = {
    settings = {
      experimental-features = ["nix-command" "flakes"];
      trusted-users = ["root" "@wheel"];
    };
    gc = {
      automatic = true;
      options = "--delete-older-than 7d";
    };
  };

  # System packages
  environment.systemPackages = with pkgs; [
    wget
    curl
    git
    vim
    htop
  ];

  # Enable programs
  programs = {
    bash.completion.enable = true;
    git.enable = true;
  };

  # User configuration
  users.users.kei = {
    isNormalUser = true;
    home = "/home/kei";
    extraGroups = ["wheel"];
    shell = pkgs.bash;
  };

  # Security
  security.sudo.wheelNeedsPassword = false;

  system.stateVersion = "24.05";
}
