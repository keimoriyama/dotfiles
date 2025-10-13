{inputs}:let
  system = "aarch64-darwin";
  pkgs = import inputs.nixpkgs {inherit system;};
  username = "kei";

in
  
