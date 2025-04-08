{
  nixpkgs,
  home-manager,
  emacs-overlay,
  org-babel,
  system,
  neovim-nightly-overlay,
  ...
}: let
  username = "moriyamakei";
  sources = pkgs.callPackage ../_sources/generated.nix {};

  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = import ./overlay/default.nix {
      inherit emacs-overlay;
      inherit neovim-nightly-overlay;
    };
  };
  emacs = import ./packages/emacs {
    inherit (nixpkgs) lib;
    inherit pkgs sources;
  };
  python = import ./packages/python {
    inherit (nixpkgs) lib;
    inherit pkgs;
  };
  emacsPkg = emacs.emacs-stable-without-nativecomp;
  defaultPrograms = import ./programs/default.nix {
    inherit pkgs;
    inherit org-babel emacsPkg;
  };
in {
  imports = defaultPrograms;

  home = {
    username = username;
    homeDirectory = "/Users/${username}";

    stateVersion = "25.05";
    # packages = defaultPackages;
    packages = with pkgs; [
      git
      curl
      uv
      nodejs_23
      python313
      alejandra
      fish
      peco
      ghq
      gh
      deno
      wezterm
      ripgrep
      neovim
      vim
      tree-sitter
      pyright
      ruff
      yaml-language-server
      lua-language-server
      docker
    ];
  };

  programs.git = {
    enable = true;
    userName = "keimoriyama";
    userEmail = "keischwiiz@gmail.com";
  };

  programs.fish = {
    enable = true;
    plugins = [
      {
        name = "z";
        src = pkgs.fishPlugins.z.src;
      }
    ];
    shellAliases = {
      g = "git";
      ga = "git add";
      gd = "git diff";
      gp = "git push";
      gpo = "git push origin";
      gb = "git branch";
      gs = "git status";
      gco = "git checkout";
      gf = "git fetch";
      gc = "git commit";
      c = "clear";
      n = "nvim";
      python = "python3";
    };
    functions = {
      fish_prompt = "
    echo -n (set_color cyan)(prompt_pwd) (set_color red)'❯'(set_color yellow)'❯'(set_color green)'❯ '
";
      fish_right_prompt = "
	printf \"%s\" (__fish_git_prompt)
	";
    };
    loginShellInit = "
set -x PATH \"/nix/var/nix/profiles/default/bin\" \"$PATH\"
set -x PATH \"$HOME/.nix-profile/bin\" \"$PATH\"
set -x PATH \"/opt/homebrew/bin\" \"$PATH\"
# Fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red

# Status Chars
set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_untrackedfiles '☡'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '+'
set __fish_git_prompt_char_upstream_behind '-'
";
    interactiveShellInit = "
set -gx MOCWORD_DATA $HOME/.local/mocword-data/mocword.sqlite
set -gx DPP_PATH $HOME/.cache/dpp
set -gx HYDRA_FULL_ERROR 1
set -gx NIX_USER_CONF_FILES $XDG_CONFIG_HOME/nix/nix.conf:$$XDG_CONFIG_HOME/nix/local.conf;
";
  };
  programs.home-manager.enable = true;
}
