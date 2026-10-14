{
  pkgs,
  sources,
}:
pkgs.rustPlatform.buildRustPackage {
  name = "yaskkserv2";
  src = sources.yaskkserv2.src;
  # 上流の Cargo.lock は脆弱な依存を含むため、cargo update 済みのものに差し替える。
  cargoLock.lockFile = ./Cargo.lock;
  # 上流は mio の feature を tokio 経由の有効化に頼っており、tokio 1.39 以降で壊れるため明示する。
  postPatch = ''
    cp ${./Cargo.lock} Cargo.lock
    substituteInPlace Cargo.toml \
      --replace-fail 'mio = "0.8.3"' 'mio = { version = "0.8.3", features = ["os-poll", "net"] }'
  '';
  meta = with pkgs.lib; {
    description = "A Japanese input method server compatible with SKK";
  };
  doCheck = false;
  nativeBuildInputs = [
    pkgs.pkg-config
  ];
  buildInputs = [
    pkgs.openssl
  ];
}
