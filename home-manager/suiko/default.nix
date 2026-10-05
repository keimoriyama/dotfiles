{
  pkgs,
  suiko,
}: let
  cargoToml = builtins.fromTOML (builtins.readFile "${suiko}/Cargo.toml");
  # build.rs は既定で DICT_ZIP_URL から辞書zipを取得し DICT_ZIP_SHA256 で検証する。
  # Nixのサンドボックスビルドはネットワークにアクセスできないため、同じ定数を
  # build.rs から読み取って fetchurl で事前取得し、SUIKO_SUDACHI_DICT 経由で渡す。
  buildRs = builtins.readFile "${suiko}/build.rs";
  rustConst = name: let
    m = builtins.match ''.*const ${name}: &str = "([^"]+)";.*'' buildRs;
  in
    if m == null
    then throw "suiko: build.rs に ${name} が見つからない"
    else builtins.head m;
  sudachiDictZip = pkgs.fetchurl {
    url = rustConst "DICT_ZIP_URL";
    sha256 = rustConst "DICT_ZIP_SHA256";
  };
in
  pkgs.rustPlatform.buildRustPackage {
    pname = "suiko";
    inherit (cargoToml.package) version;
    src = suiko;

    cargoLock.lockFile = "${suiko}/Cargo.lock";

    nativeBuildInputs = [pkgs.unzip];

    preBuild = ''
      unzip -p ${sudachiDictZip} '${rustConst "DICT_ZIP_ENTRY"}' > "$NIX_BUILD_TOP/system_core.dic"
      export SUIKO_SUDACHI_DICT="$NIX_BUILD_TOP/system_core.dic"
    '';

    meta = with pkgs.lib; {
      description = "Deterministic diagnostics for natural and readable Japanese writing";
      homepage = "https://github.com/nwiizo/suiko";
      license = licenses.mit;
      mainProgram = "suiko";
    };
  }
