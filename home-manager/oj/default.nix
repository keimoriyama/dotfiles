{
  pkgs,
  sources,
}:
pkgs.python3Packages.buildPythonPackage rec {
  pname = "online-judge-tools";
  version = sources.oj.version;
  pyproject = true;
  src = sources.oj.src;

  build-system = [pkgs.python3Packages.setuptools];

  dependencies = with pkgs.python3Packages; [
    colorama
    online-judge-api-client
    packaging
    requests
  ];

  pythonImportsCheck = [
    "onlinejudge"
    "onlinejudge_command"
  ];

  # ネットワークアクセスが必要なテストが含まれるため無効化
  doCheck = false;

  meta = with pkgs.lib; {
    description = "Tools for various online judges. Download sample cases, generate additional test cases, test your code, and submit it";
    mainProgram = "oj";
    homepage = "https://github.com/online-judge-tools/oj";
    license = licenses.mit;
  };
}
