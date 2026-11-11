{
  pkgs,
  lib,
  inputs,
  isWork ? false,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) system isDarwin;

  opencodeSandbox = pkgs.callPackage ./opencode/sandbox-plugin.nix {};

  # LIMITATION: Bash ツールを OS で隔離するのは macOS だけ。Linux では bubblewrap と ripgrep が
  # 要るが未検証なので、external_directory の制限しか効かない。
  opencodeConfig =
    lib.importJSON ./opencode/opencode.json
    // lib.optionalAttrs isDarwin {
      plugin = ["file://${opencodeSandbox}/lib/node_modules/opencode-sandbox/dist/index.js"];
    };
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
  xdg.configFile."opencode/opencode.json".text = builtins.toJSON opencodeConfig;

  # Bash ツールはホーム配下では ~/ghq だけを読み書きでき、通信先は許可リストに限る。
  # 設定はプロジェクト外に置き、サンドボックス内のコマンドから書き換えられないようにする。
  xdg.configFile."opencode-sandbox/config.json" = lib.mkIf isDarwin {
    source = ./opencode/sandbox-config.json;
  };

  # agent-shell の Sarashina チャットが OPENCODE_CONFIG で渡す設定。
  # ファイル名が opencode.json ではないので、素の opencode は読み込まない。
  xdg.configFile."opencode/sarashina.json".source = ./opencode/sarashina.json;
}
