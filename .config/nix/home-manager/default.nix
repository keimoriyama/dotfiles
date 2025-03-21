{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: let
  username = "kei";
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

  home = {
    username = username;
    homeDirectory = "/Users/${username}";

    stateVersion = "25.05";
    packages = with pkgs; [
      git
      curl
      uv
      nodejs_23
      alejandra
      fish
      fishPlugins.z
      deno
      emacs
      neovim
      vim
      tree-sitter
      pyright
      ruff
      wezterm
      yaml-language-server
      lua-language-server
    ];
  };

  programs.home-manager.enable = true;
}
