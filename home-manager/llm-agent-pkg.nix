{
  pkgs,
  lib,
  inputs,
  isWork ? false,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) system isDarwin;

  # opencode の Bash ツールが使うシェル。ファイル名で引数の渡し方が決まるので zsh にする。
  # PATH には入れず、opencode.json の shell からだけ参照する。
  sandboxedShell = pkgs.writeShellScriptBin "zsh" ''
    exec /usr/bin/sandbox-exec -f ${./opencode/shell-sandbox.sb} -D HOME="$HOME" /bin/zsh "$@"
  '';

  # LIMITATION: Bash ツールを OS で隔離するのは macOS だけ。Linux では external_directory の
  # 制限しか効かない。
  opencodeConfig =
    lib.recursiveUpdate
    (lib.importJSON ./opencode/opencode.json)
    (lib.optionalAttrs isDarwin {shell = "${sandboxedShell}/bin/zsh";});
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

  # agent-shell の Sarashina チャットが OPENCODE_CONFIG で渡す設定。
  # ファイル名が opencode.json ではないので、素の opencode は読み込まない。
  xdg.configFile."opencode/sarashina.json".source = ./opencode/sarashina.json;
}
