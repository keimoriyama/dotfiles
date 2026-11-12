# nixpkgs に無いツール。
{
  pkgs,
  lib,
  inputs,
  ...
}: let
  arto = pkgs.stdenvNoCC.mkDerivation rec {
    pname = "arto";
    version = "0.43.1";

    src = pkgs.fetchurl {
      url = "https://github.com/arto-app/Arto/releases/download/v${version}/Arto_${version}_aarch64.dmg";
      hash = "sha256-DndXMsmTKSMRQoUiRx2XX4skcmFK2T3EWMb0GistupQ=";
    };

    nativeBuildInputs = [pkgs._7zz];
    sourceRoot = ".";

    unpackPhase = ''
      runHook preUnpack
      7zz x -snld20 "$src"
      runHook postUnpack
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p "$out/Applications"
      cp -R Arto.app "$out/Applications/"
      runHook postInstall
    '';
  };
in {
  home.packages =
    [
      (pkgs.callPackage ./suiko {inherit (inputs) suiko;})
    ]
    ++ lib.optional pkgs.stdenv.hostPlatform.isDarwin arto;
}
