# {
#   pkgs,
#   sources,
# } :
# pkgs.rustPlatform.buildRustPackage  {
#   pname = "yaskkserv2";
#   src = sources.yaskkserv2.src;
#   cargBuildFlags = ["--release"];
#   meta = with pkgs.lib; {
#     description = "A Japanese input method server compatible with SKK";
#   };
#   }
