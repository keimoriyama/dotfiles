{
  pkgs,
  sources,
}:
pkgs.rustPlatform.buildRustPackage {
  pname = "kakehashi";
  version = sources.kakehashi.version;
  src = sources.kakehashi.src;
  cargoLock = {
    lockFile = "${sources.kakehashi.src}/Cargo.lock";
  };
  doCheck = false;
  meta = with pkgs.lib; {
    description = "Tree-sitter based language server for host and embedded languages";
    homepage = "https://github.com/atusy/kakehashi";
    license = licenses.mit;
  };
}
