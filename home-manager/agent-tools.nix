# AI エージェントに自律作業をさせるための周辺ツール。
# 制限系 (cage / guard-and-guide) と、動作確認・観測系 (agent-browser / tu / zmx / agtlog)。
{
  pkgs,
  system,
  cage,
  guard-and-guide,
  agtlog,
  terminalUse,
}:
[
  # 書き込みを OS のサンドボックス (macOS: Apple Seatbelt, Linux: Landlock) で縛る。
  cage.packages.${system}.default
  # PreToolUse hook から呼ばれ、危険な操作をブロックして代替手段を提示する。
  guard-and-guide.packages.${system}.default
  # セッションログをセッション外のビューアで読む。focus モードで隠れる情報を追うため。
  agtlog.packages.${system}.default
  # TUI / 対話的 CLI をエージェントに操作させる (コマンド名は tu)。
  terminalUse
]
++ (with pkgs; [
  # ブラウザ越しの動作確認をエージェント自身にやらせる。
  agent-browser
  # 開発サーバーなど長寿命プロセスのセッション管理。外からアタッチして確認できる。
  zmx
])
