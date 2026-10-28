# nixpkgs に無い、または flake input からビルドするツール。
{
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.packages =
    [
      (pkgs.callPackage ./suiko {inherit (inputs) suiko;})
    ]
    ++ lib.optional pkgs.stdenv.hostPlatform.isDarwin
    inputs.arto.packages.${pkgs.stdenv.hostPlatform.system}.default;
}
