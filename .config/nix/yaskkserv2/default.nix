{
  pkgs,
  sources,
}:
pkgs.rustPlatform.buildRustPackage {
  name = "yaskkserv2";
  src = sources.yaskkserv2.src;
  cargBuildFlags = ["--release"];
  cargoLock = sources.yaskkserv2.cargoLock."Cargo.lock";
  meta = with pkgs.lib; {
    description = "A Japanese input method server compatible with SKK";
  };
  doCheck=false;
}

