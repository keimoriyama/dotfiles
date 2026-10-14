{
  pkgs,
  sources,
}:
pkgs.rustPlatform.buildRustPackage {
  name = "cargo-compete";
  src = sources.cargo-compete.src;
  # 上流の Cargo.lock は脆弱な依存を含むため、cargo update 済みのものに差し替える。
  cargoLock.lockFile = ./Cargo.lock;
  postPatch = ''
    cp ${./Cargo.lock} Cargo.lock
  '';
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
