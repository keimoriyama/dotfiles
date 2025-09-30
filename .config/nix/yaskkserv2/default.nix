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
# {
#   pkgs
# }:
# pkgs.stdenv.mkDerivation {
#   pname = "yaskkserv2";
#   version = "v0.1.7";
#   src = pkgs.fetchzip {
#     url = "https://github.com/wachikun/yaskkserv2/releases/download/0.1.7/yaskkserv2-0.1.7-x86_64-apple-darwin.tar.gz";
#     sha256 = "0palb1wf3f935zc2wn2l2163s4b400l9gkdq2pgpqj3kg80l3j23";
#   };
#
#   installPhase = ''
# 	mkdir -p $out/bin
# 	cp yaskkserv2 $out/bin
# 	cp yaskkserv2_make_dictionary $out/bin
# '';
#
# }

