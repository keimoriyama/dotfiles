{
  pkgs,
  sources,
}:
pkgs.rustPlatform.buildRustPackage {
  name = "yaskkserv2";
  src = sources.yaskkserv2.src;
  cargBuildFlags = ["--release"];
  cargoHash = "sha256-cycs8Zism228rjMaBpNYa4K1Ll760UhLKkoTX6VJRU0=";
  meta = with pkgs.lib; {
    description = "A Japanese input method server compatible with SKK";
  };
  doCheck = false;
}
