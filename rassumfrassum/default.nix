{pkgs}:
pkgs.python3Packages.buildPythonPackage rec {
  pname = "rassumfrassum";
  version = "0.2.1";

  src = pkgs.fetchPypi {
    inherit pname version;
    sha256 = "sha256-m0/mTeyHdF0TswkQscfogeanbQiR+mYCDxTI6RbF7DY=";
  };
  pyproject = true;
  buildInputs = [pkgs.python3.pkgs.setuptools];
}
