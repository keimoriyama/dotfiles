# nixpkgs に無い、または flake input からビルドするツール。
{
  pkgs,
  lib,
  inputs,
  sources,
  ...
}: {
  home.packages =
    [
      (pkgs.callPackage ./cargo-compete {inherit sources;})
      (pkgs.callPackage ./kakehashi {inherit sources;})
      (pkgs.callPackage ./nippo {inherit (inputs) nippo;})
      (pkgs.callPackage ./suiko {inherit (inputs) suiko;})
    ]
    ++ lib.optional pkgs.stdenv.hostPlatform.isDarwin
    inputs.arto.packages.${pkgs.stdenv.hostPlatform.system}.default;
}
