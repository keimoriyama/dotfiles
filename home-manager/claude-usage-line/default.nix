# Claude Code の statusline。コンテキスト使用率・レート制限・セッション費用を出す。
# Enterprise では stdin にレート制限が来ないので、OAuth で usage API に問い合わせる。
{
  pkgs,
  sources,
}:
pkgs.stdenvNoCC.mkDerivation {
  # npm の公開物を nvfetcher で追従する。
  inherit (sources.claude-usage-line) pname version src;

  nativeBuildInputs = [pkgs.makeWrapper];

  # dist/ はコンパイル済みで実行時依存も無いため、置いて node で起動するだけ。
  installPhase = ''
    runHook preInstall

    mkdir -p "$out/lib/claude-usage-line"
    cp -r . "$out/lib/claude-usage-line"

    makeWrapper "${pkgs.nodejs_24}/bin/node" "$out/bin/claude-usage-line" \
      --add-flags "$out/lib/claude-usage-line/dist/cli.js"

    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "Claude Code status line showing context usage, rate limits and session cost";
    homepage = "https://github.com/canack/claude-usage-line";
    license = licenses.mit;
    mainProgram = "claude-usage-line";
    platforms = platforms.unix;
  };
}
