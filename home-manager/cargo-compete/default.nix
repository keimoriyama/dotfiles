{
  pkgs,
  sources,
}:
pkgs.rustPlatform.buildRustPackage {
  name = "cargo-compete";
  src = sources.cargo-compete.src;
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
