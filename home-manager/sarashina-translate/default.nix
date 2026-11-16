{pkgs}:
pkgs.rustPlatform.buildRustPackage {
  pname = "sarashina-translate";
  version = "0.1.0";
  src = ./.;

  cargoLock.lockFile = ./Cargo.lock;

  meta = with pkgs.lib; {
    description = "CLI client for the Sarashina Translate API";
    homepage = "https://docs.sarashina.ai/api-reference/translate";
    license = licenses.mit;
    mainProgram = "sarashina-translate";
    platforms = platforms.unix;
  };
}
