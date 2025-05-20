{
  lib,
  pkgs,
}: let
  override = final: prev: {
    dap-mode = prev.melpaPackages.dap-mode.overrideAttrs (old: {
      preBuild = null;
    });
  };
in {
  emacs-stable = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs;
    extraEmacsPackages = import ./epkgs {inherit pkgs;};
    override = override;
  };

  emacs-stable-without-nativecomp = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs.override {
      withNativeCompilation = false;
    };
    extraEmacsPackages = import ./epkgs {inherit pkgs;};
    override = override;
  };
  emacs-git = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs-git.overrideAttrs (old: {
      buildInputs =
        old.buildInputs
        ++ lib.optional pkgs.stdenv.isDarwin [pkgs.darwin.apple_sdk.frameworks.WebKit];
      configureFlags = old.configureFlags ++ ["--with-xwidgets"];
    });
    extraEmacsPackages = import ./epkgs {inherit pkgs;};
    override = override;
  };

  emacs-unstable-without-nativecomp = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.toFile "empty.el" "";
    package = pkgs.emacs-unstable.override {
      withNativeCompilation = false;
    };
    extraEmacsPackages = import ./epkgs {inherit pkgs;};
    override = override;
  };
}
