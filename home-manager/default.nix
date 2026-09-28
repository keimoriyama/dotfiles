{
  pkgs,
  lib,
  username,
  ...
}: {
  imports = [
    ./packages.nix
    ./utils.nix
    ./langs.nix
    ./gui.nix
    ./darwin.nix
    ./llm-agent-pkg.nix
    ./agent-tools.nix
    ./skk.nix
    ./wezterm
    ./emacs
    ./fish
    ./git
    ./nh
    ./claude-code
    ./agents
    ./agent-skills.nix
    ./textlint
  ];

  # nvfetcher で取得したソース。fish / emacs / 自前ビルドのパッケージが参照する。
  _module.args.sources = pkgs.callPackage ../_sources/generated.nix {};

  programs.home-manager.enable = true;
  home = {
    stateVersion = "26.05";
    inherit username;
    homeDirectory = lib.mkDefault (
      if pkgs.stdenv.hostPlatform.isDarwin
      then "/Users/${username}"
      else "/home/${username}"
    );

    sessionVariables = {
      EDITOR = "vim";
    };
  };
}
