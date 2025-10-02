{pkgs, sources}:
pkgs.buildNpmPackage {
  pname="textlint";
  version=sources.textlint.version;
  src=sources.textlint.src;
}
