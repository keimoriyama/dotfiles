{
  pkgs,
  sources,
}:
pkgs.rustPlatform.buildRustPackage {
  pname = "terminal-use";
  version = sources.terminal-use.version;
  src = sources.terminal-use.src;
  cargoLock = {
    lockFile = "${sources.terminal-use.src}/Cargo.lock";
  };
  doCheck = false;
  meta = with pkgs.lib; {
    description = "tu is tmux for your coding agent";
    homepage = "https://github.com/flipbit03/terminal-use";
    license = licenses.mit;
    mainProgram = "tu";
  };
}
