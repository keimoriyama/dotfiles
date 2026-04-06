{
  pkgs,
  sources,
}:
pkgs.rustPlatform.buildRustPackage {
  name = "carg-compete";
  src = sources.cargo-compete.src;
  # cargoLock = sources.cargo-compete.cargoLock."Cargo.lock";
  # cargoLock = {
  #   lockFile = ./Cargo.lock;
  # };
  cargoHash = "sha256-lid1tyR8Y6lvjpeGJ4vGzqDTY6V2y/5rL9fGyjyF3yw=";
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
