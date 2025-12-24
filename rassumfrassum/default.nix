{pkgs}:
pkgs.buildPythonPackage rec {
  pname = "example-package";
  version = "1.0.0";

  src = pkgs.fetchPypi {
    inherit pname version;
    sha256 = "0v..."; # Replace with actual sha256
  };

  buildInputs = [pkgs.python3.pkgs.setuptools];
}
