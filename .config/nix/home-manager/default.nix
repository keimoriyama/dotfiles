{
  inputs,
  lib,
  config,
  pkgs,
  org-babel,
  ...
}: let
  username = "kei";
  emacs = import ./packages/emacs {
    inherit lib;
    inherit pkgs;
  };
  emacsPkg = emacs.emacs-stable;
  defaultPrograms = import ./programs/default.nix {
    inherit pkgs;
    inherit org-babel emacsPkg;
  };
in {
  nixpkgs = {
    overlays = [
      (final: prev: {
        vim = prev.vim.overrideAttrs (oldAttrs: {
          version = "latest";
          src = inputs.vim-src;
          configureFlags =
            oldAttrs.configureFlags
            ++ [
              "--enable-terminal"
              "--with-compiledby=nix-home-manager"
              "--enable-luainterp"
              "--with-lua-prefix=${prev.lua}"
              "--enable-fail-if-missing"
            ];
          buildInputs =
            oldAttrs.buildInputs
            ++ [prev.gettext prev.lua prev.libiconv];
        });
      })
      inputs.neovim-nightly-overlay.overlays.default
    ];
    config = {
      allowUnfree = true;
    };
  };

  imports = defaultPrograms;

  home = {
    username = username;
    homeDirectory = "/Users/${username}";

    stateVersion = "25.05";
    # packages = defaultPackages;
    packages = with pkgs;
      [
        git
        curl
        uv
        nodejs_23
        alejandra
        fish
        fishPlugins.z
        deno
        python3Full
        wezterm
        ripgrep
        # emacs
        neovim
        vim
        tree-sitter
        pyright
        ruff
        yaml-language-server
        lua-language-server
      ]
      + emacsPkg;
  };
  programs.home-manager.enable = true;
}
