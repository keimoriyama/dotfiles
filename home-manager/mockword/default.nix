{
  pkgs,
  sources,
}:
pkgs.rustPlatform.buildRustPackage {
  name = "mocword";
  src = sources.mocword.src;
  cargoLock = {
    lockFile = ./Cargo.lock;
  };
  postPatch = ''
    ln -s ${./Cargo.lock} Cargo.lock
  '';
}
