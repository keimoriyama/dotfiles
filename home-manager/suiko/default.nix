{
  pkgs,
  suiko,
}: let
  cargoToml = builtins.fromTOML (builtins.readFile "${suiko}/Cargo.toml");
  # build.rs は既定でこのURLから辞書zipを取得しSHA-256で検証する。Nixの
  # サンドボックスビルドはネットワークにアクセスできないため、fetchurlで
  # 事前取得したzipを SUIKO_SUDACHI_DICT 経由で渡す。URL・ハッシュは
  # suiko の build.rs (DICT_ZIP_URL / DICT_ZIP_SHA256) と同期させること。
  sudachiDictZip = pkgs.fetchurl {
    url = "https://d2ej7fkh96fzlu.cloudfront.net/sudachidict/sudachi-dictionary-20260723-core.zip";
    sha256 = "b6e835f63440f97474c2da45d80950f73746e632e40bbfc168b4041729135e1f";
  };
in
  pkgs.rustPlatform.buildRustPackage {
    pname = "suiko";
    inherit (cargoToml.package) version;
    src = suiko;

    cargoLock.lockFile = "${suiko}/Cargo.lock";

    nativeBuildInputs = [pkgs.unzip];

    preBuild = ''
      unzip -p ${sudachiDictZip} '*/system_core.dic' > "$NIX_BUILD_TOP/system_core.dic"
      export SUIKO_SUDACHI_DICT="$NIX_BUILD_TOP/system_core.dic"
    '';

    meta = with pkgs.lib; {
      description = "Deterministic diagnostics for natural and readable Japanese writing";
      homepage = "https://github.com/nwiizo/suiko";
      license = licenses.mit;
      mainProgram = "suiko";
    };
  }
