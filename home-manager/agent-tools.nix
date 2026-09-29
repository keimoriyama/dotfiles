# AI エージェントに自律作業をさせるための周辺ツール。
# 制限系 (guard-and-guide) と観測系 (cclens / claude-usage-line)。
{
  pkgs,
  inputs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) system;
in {
  home.packages = [
    # PreToolUse hook から呼ばれ、危険な操作をブロックして代替手段を提示する。
    inputs.guard-and-guide.packages.${system}.default
    # transcript と設定をローカルで集計し、利用状況や失敗傾向を診断する。
    inputs.cclens.packages.${system}.default
    # statusLine から呼ばれ、コンテキスト使用率とレート制限を 1 行で出す。
    (pkgs.callPackage ./claude-usage-line {})
  ];
}
