{
  pkgs,
  sources,
}:
pkgs.rustPlatform.buildRustPackage {
  name = "carg-compete";
  src = sources.cargo-compete.src;
  cargoLock = sources.cargo-compete.cargoLock."Cargo.lock";
  doCheck = false;
  nativeBuildInputs = [
    pkgs.pkg-config
  ];
  buildInputs = [
    pkgs.openssl
    pkgs.zlib
  ];
  buildNoDefaultFeatures = true;
}
