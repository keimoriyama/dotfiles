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
    inherit pkgs;
  };
  python = import ./packages/python {
    inherit (nixpkgs) lib;
    inherit pkgs;
  };
  emacsPkg = emacs.emacs-stable;
  defaultPrograms = import ./programs/default.nix {
    inherit pkgs;
    inherit org-babel emacsPkg;
  };
  sources = pkgs.callPackage ../_sources/generated.nix {};
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
      # fishPlugins.z
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
      if git rev-parse --is-inside-work-tree > /dev/null 2>&1
          string join '' --  (prompt_pwd)(fish_git_prompt) \">\"
        else

                string join '' --  (prompt_pwd) \">\"
        end
";
    };
    loginShellInit = "
set -x PATH \"/nix/var/nix/profiles/default/bin\" \"$PATH\"
set -x PATH \"$HOME/.nix-profile/bin\" \"$PATH\"
";
    interactiveShellInit = "
set -gx MOCWORD_DATA $HOME/.local/mocword-data/mocword.sqlite
set -gx DPP_PATH $HOME/.cache/dpp
set -gx HYDRA_FULL_ERROR 1
";
  };
  programs.home-manager.enable = true;
}
