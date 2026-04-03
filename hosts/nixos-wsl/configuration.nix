{
  config,
  pkgs,
  lib,
  ...
}: {
  # Additional system configuration specific to your WSL setup
  # Add any custom configuration here

  # Example: Enable specific services
  # services.openssh.enable = true;

  # Example: Install additional packages
  # environment.systemPackages = with pkgs; [
  #   neovim
  #   tmux
  # ];
}
