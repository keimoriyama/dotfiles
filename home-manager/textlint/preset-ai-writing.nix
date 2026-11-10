# AI らしい記述パターンを検出する textlint プリセット。nixpkgs に無いため自前でビルドする。
# upstream の package-lock.json は resolved が欠けた項目が多くオフラインで取得できない。
# そこでビルド済みの npm 公開物を使い、実行時依存だけの lockfile を同梱する。
# 更新時は公開物の package.json から devDependencies と scripts を消し、
# `npm install --package-lock-only` で preset-ai-writing-lock.json を作り直す。
{
  lib,
  buildNpmPackage,
  fetchurl,
  jq,
}:
buildNpmPackage (finalAttrs: {
  pname = "textlint-rule-preset-ai-writing";
  version = "1.7.0";

  src = fetchurl {
    url = "https://registry.npmjs.org/@textlint-ja/textlint-rule-preset-ai-writing/-/textlint-rule-preset-ai-writing-${finalAttrs.version}.tgz";
    hash = "sha256-tqcUCTJ9nWRjXHiNjEV3nZMLaotOLKLYiTl4V32Jtvk=";
  };

  postPatch = ''
    ${lib.getExe jq} 'del(.devDependencies, .scripts)' package.json > package.json.new
    mv package.json.new package.json
    cp ${./preset-ai-writing-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-C/2ZvuLlVPJjEQgFMkNoqm9lWb6WejdaAcJv/Rs1/FE=";

  dontNpmBuild = true;

  meta = {
    description = "textlint preset that detects AI-like writing patterns in Japanese";
    homepage = "https://github.com/textlint-ja/textlint-rule-preset-ai-writing";
    license = lib.licenses.mit;
  };
})
