{pkgs}:
pkgs.rustPlatform.buildRustPackage {
  pname = "sarashina-cli";
  version = "0.1.0";
  src = ./.;

  cargoLock.lockFile = ./Cargo.lock;

  postInstall = ''
    ln -s sarashina $out/bin/sarashina-translate
    ln -s sarashina $out/bin/sarashina-proofread
  '';

  meta = with pkgs.lib; {
    description = "CLI client for Sarashina translation and Japanese proofreading";
    homepage = "https://docs.sarashina.ai/";
    license = licenses.mit;
    mainProgram = "sarashina";
    platforms = platforms.unix;
  };
}
