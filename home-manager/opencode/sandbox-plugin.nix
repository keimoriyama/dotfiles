# opencode の Bash ツールを @anthropic-ai/sandbox-runtime で隔離するプラグイン。
# opencode.json に npm 名を書くと起動時に npm から取ってくるので、Nix でビルドしてパスで読ませる。
# ビルド済みの npm 公開物を使い、実行時依存だけの lockfile を同梱する。
# 更新時は version と hash を上げ、公開物の package.json から devDependencies、scripts、
# peerDependencies を消して `npm install --package-lock-only` で sandbox-plugin-lock.json を作り直す。
{
  lib,
  buildNpmPackage,
  fetchurl,
  importNpmLock,
}: let
  packageLock = lib.importJSON ./sandbox-plugin-lock.json;
in
  buildNpmPackage {
    pname = "opencode-sandbox";
    inherit (packageLock) version;

    src = fetchurl {
      url = "https://registry.npmjs.org/opencode-sandbox/-/opencode-sandbox-${packageLock.version}.tgz";
      hash = "sha256-hDKIwQx/dtZHGUKkfCU0HsziIERlL20batRFGXtb2Uo=";
    };

    # npm ci は下の package と lockfile で依存を入れるので、devDependencies は入らない。
    npmDeps = importNpmLock {
      package = {inherit (packageLock.packages."") name version dependencies;};
      inherit packageLock;
    };
    inherit (importNpmLock) npmConfigHook;

    dontNpmBuild = true;
    # 公開物の package.json に残る devDependencies をオフラインで解決しようとして失敗する。
    # 依存は lockfile の時点で実行時のものだけなので prune は要らない。
    dontNpmPrune = true;

    meta = {
      description = "OpenCode plugin that sandboxes bash commands with @anthropic-ai/sandbox-runtime";
      homepage = "https://github.com/isanchez31/opencode-sandbox-plugin";
      license = lib.licenses.mit;
    };
  }
