{pkgs}:
pkgs.stdenv.mkDerivation{
	name="mocword-data";
	src = pkgs.lib.fetchurl{
	url = "https://github.com/high-moctane/mocword-data/releases/download/eng20200217/mocword.sqlite.gz";
	hash="";
	};
	installPhase = ''
    # Create the output directory
    mkdir -p $out/share

    # Gzip the source file and place it in the output
    gzip -c $src > $out/share/mocword.sqlite
  '';
}
