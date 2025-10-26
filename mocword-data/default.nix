{pkgs}:
pkgs.stdenv.mkDerivation {
  name = "mocword-data";
  src = pkgs.fetchzip {
    # extension="gzip";
    url = "https://github.com/high-moctane/mocword-data/releases/download/eng20200217/mocword.sqlite.gz";
    hash = "";
  };
}
