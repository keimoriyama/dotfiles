{
  pkgs,
  nippo,
}: let
  cargoToml = builtins.fromTOML (builtins.readFile "${nippo}/crates/collector/Cargo.toml");
in
  pkgs.rustPlatform.buildRustPackage {
    pname = "nippo";
    inherit (cargoToml.package) version;
    src = nippo;

    cargoLock.lockFile = "${nippo}/Cargo.lock";

    nativeBuildInputs = [pkgs.pkg-config];
    buildInputs = [pkgs.sqlite];

    meta = with pkgs.lib; {
      description = "Generate daily reports from Claude Code and Codex work logs";
      homepage = "https://github.com/nwiizo/nippo";
      license = licenses.mit;
      mainProgram = "nippo";
    };
  }
