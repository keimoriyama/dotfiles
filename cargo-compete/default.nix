{pkgs}:
pkgs.rustPlatform.buildRustPackage {
  name = "carg-compete";
  src = let
    source = pkgs.fetchFromGitHub {
      owner = "qrxip";
      repo = "carg-compete";
      rev = "v0.10.7";
      sha256 = "sha256-+X5k1Y1p3b2";
    };
  in
    pkgs.runCommand "source" {} ''
      mkdir -p $out
      cp -r ${source}/* $out/
      cp ${./Cargo.lock} $out/Cargo.lock
    '';
}
