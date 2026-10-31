{
  pkgs,
  lib,
  config,
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
    ./zsh
    ./git
    ./nh
    ./claude-code
    ./agents
    ./agent-skills.nix
    ./textlint
  ];

  # nvfetcher で取得したソース。emacs / 自前ビルドのパッケージが参照する。
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

    # zsh が hm-session-vars 経由で読み込む。
    sessionVariables = {
      EDITOR = "vim";
      MOCWORD_DATA = "${config.home.homeDirectory}/.local/mocword-data/mocword.sqlite";
      DPP_PATH = "${config.home.homeDirectory}/.cache/dpp";
      LSP_USE_PLISTS = "1";
      HYDRA_FULL_ERROR = "1";
      NIX_USER_CONF_FILES = "${config.xdg.configHome}/nix/nix.conf:${config.xdg.configHome}/nix/local.conf";
    };
  };
}
