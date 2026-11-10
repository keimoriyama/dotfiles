{
  pkgs,
  lib,
  inputs,
  isWork ? false,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) system;
in {
  home.packages = with inputs.llm-agents.packages.${system};
    [
      claude-code
      claude-agent-acp
      opencode
      pi
    ]
    # 業務用マシンでは codex 系を入れない。
    ++ lib.optional (!isWork) codex-acp;

  # プロジェクト外のパスは ~/ghq とスキル置き場だけ許す。
  xdg.configFile."opencode/opencode.json".source = ./opencode/opencode.json;

  # agent-shell の Sarashina チャットが OPENCODE_CONFIG で渡す設定。
  # ファイル名が opencode.json ではないので、素の opencode は読み込まない。
  xdg.configFile."opencode/sarashina.json".source = ./opencode/sarashina.json;
}
