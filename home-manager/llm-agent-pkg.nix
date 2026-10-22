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
}
