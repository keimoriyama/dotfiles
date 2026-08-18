{
  pkgs,
  sources,
}:
pkgs.buildNpmPackage rec {
  pname = "atcoder-cli";
  version = sources.atcoder-cli.version;
  src = sources.atcoder-cli.src;

  npmDepsHash = "sha256-ufG7Fq5D2SOzUp8KYRYUB5tYJYoADuhK+2zDfG0a3ks=";

  # fsevents は macOS 専用の任意依存で、webpack の watch モードでのみ使われる。
  # 古い node-gyp バインディングが現行 V8 ヘッダでビルドできず失敗するため除外する。
  npmFlags = ["--omit=optional"];

  # webpack 4 が使う md4 ハッシュは OpenSSL 3 でサポートされなくなったため、
  # レガシープロバイダを有効にする必要がある。
  NODE_OPTIONS = "--openssl-legacy-provider";

  dontNpmBuild = false;

  meta = with pkgs.lib; {
    description = "AtCoder command line tools";
    mainProgram = "acc";
    homepage = "https://github.com/Tatamo/atcoder-cli";
    license = licenses.bsd3;
  };
}
