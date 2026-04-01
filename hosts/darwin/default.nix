{pkgs, ...}: let
  aerospace = import ../../nix-darwin/aerospace {inherit pkgs;};
in {
  imports = [
    aerospace
    ./system.nix
    ./user.nix
  ];
}
