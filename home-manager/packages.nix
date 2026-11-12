# nixpkgs に無いツール。
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
    # LIMITATION: 上流 flake は Linux でもビルドできるが、GUI アプリなので macOS にだけ入れる。
    ++ lib.optional pkgs.stdenv.hostPlatform.isDarwin
    inputs.arto.packages.${pkgs.stdenv.hostPlatform.system}.default;
}
