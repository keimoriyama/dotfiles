{
  nixpkgs,
  config,
  home-manager,
  emacs-overlay,
  org-babel,
  system,
  neovim-nightly-overlay,
  ...
}: let
  username = "kei";
  sources = pkgs.callPackage ../_sources/generated.nix {};
  lib = nixpkgs.lib;

  pkgs = import nixpkgs {
    inherit system;
    config = {
      allowUnfree = true;
      allwoUnfreePredicate = true;
    };
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
  programs.fish.enable = true;

  emacsPkg = emacs.emacs-stable;
  nodePkgs = pkgs.callPackage ../node2nix {inherit pkgs;};

  defaultPrograms = import ./programs/default.nix {
    inherit pkgs;
    inherit org-babel emacsPkg nodePkgs;
  };
in {
  imports = defaultPrograms;

  home = {
    username = username;
    homeDirectory = "/Users/${username}";

    stateVersion = "25.11";

    packages = with pkgs; [
      git
      curl
      uv
      nodejs_24
      typescript
      lua
      alejandra
      peco
      ghq
      gh
      deno
      rustc
      rustup
      ripgrep
      neovim
      vim
      tree-sitter
      pyright
      ruff
      isort
      yaml-language-server
      lua-language-server
      stylua
      typescript-language-server
      nvfetcher
      udev-gothic
      ghostscript
      wezterm
      cbc
      sl
      ispell
      typst
      tdf
      perl
      nixd
      copilot-language-server
    ];
    file = {
      ".config/nvim" = {
        target = ".config/nvim";
        source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/.config/nvim";
      };
      ".config/nix" = {
        target = ".config/nix";
        source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/.config/nix";
      };
      ".config/karabiner/karabiner.json".text = builtins.readFile ../../karabiner/karabiner.json;
    };
  };

  programs.git = {
    enable = true;
    userName = "keimoriyama";
    userEmail = "keischwiiz@gmail.com";
    ignores = [
      # macOS
      ".DS_Store"
      "._*"

      # Emacs
      "*~"
      ".#*"
      "\#*"
      "*_flymake.*"
      "flycheck_*"
      ".dir-locals-2.el"

      # Vim
      "*.swp"

      # Editors
      ".vscode"
      ".idea"

      # Tags
      "GPATH"
      "GR?TAGS"

      # Misc
      ".env"
      "*.orig"

      "*.pyc"
    ];
  };

  programs.home-manager.enable = true;
}
