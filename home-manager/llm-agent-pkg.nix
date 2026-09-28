{
  pkgs,
  lib,
  inputs,
  isWork ? false,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) system;
  cage = inputs.cage.packages.${system}.default;

  # エージェント CLI を常に cage のサンドボックス下で起動する。
  # cage は実行ファイルの basename で auto-presets を選ぶので、名前は元のまま残す。
  caged = pkg: bin:
    pkgs.symlinkJoin {
      name = "${pkg.name}-caged";
      paths = [pkg];
      nativeBuildInputs = [pkgs.makeWrapper];
      postBuild = ''
        rm $out/bin/${bin}
        makeWrapper ${cage}/bin/cage $out/bin/${bin} --add-flags ${pkg}/bin/${bin}
      '';
    };
in {
  home.packages = with inputs.llm-agents.packages.${system};
    [
      copilot-language-server
      (caged claude-code "claude")
      claude-agent-acp
      (caged opencode "opencode")
    ]
    # 業務用マシンでは codex 系を入れない。
    ++ lib.optional (!isWork) codex-acp;
}
