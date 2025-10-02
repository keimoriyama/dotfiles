{
  pkgs,
  sources,
}:
pkgs.buildNpmPackage (finalAttrs: {
  pname = "textlint";
  version = sources.textlint.version;
  src = sources.textlint.src;
  npmDepsHash = "sha256-lBSjwm3YibJKSItIZOZ+Xu7i4MTRQJMnEiqcI+TcqQs=";
  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  dependencies = with pkgs; [
    textlint-plugin-latex2e
    textlint-pluigin-org
    textlint-rule-preset-ja-technical-writing
  ];
})
