# AI エージェントに自律作業をさせるための周辺ツール。
# 制限系 (cage / guard-and-guide) と観測系 (cclens)。
{
  system,
  cage,
  guard-and-guide,
  cclens,
}: [
  # 書き込みを OS のサンドボックス (macOS: Apple Seatbelt, Linux: Landlock) で縛る。
  cage.packages.${system}.default
  # PreToolUse hook から呼ばれ、危険な操作をブロックして代替手段を提示する。
  guard-and-guide.packages.${system}.default
  # transcript と設定をローカルで集計し、利用状況や失敗傾向を診断する。
  cclens.packages.${system}.default
]
