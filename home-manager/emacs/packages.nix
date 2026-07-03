{
  pkgs,
  epkgs,
  sources,
}: {
  copilot = epkgs.melpaBuild {
    pname = "copilot";
    version = "0.0.1";
    src = sources.emacs-copilot.src;
    packageRequires = [epkgs.f];
  };
  yatex = epkgs.melpaBuild {
    pname = "yatex";
    version = "0.0.1";
    src = sources.emacs-yatex.src;
    patches = [./yatexlib.diff];
    ignoreCompilationError = true;
  };
  yaml-mode = epkgs.melpaBuild {
    pname = "yaml-mode";
    src = sources.emacs-yaml-mode.src;
    version = "0.0.1";
  };
  dmacro = epkgs.melpaBuild {
    pname = "dmacro";
    src = sources.emacs-dmacro.src;
    version = "0.0.1";
  };
  claude-code-ide = epkgs.melpaBuild {
    pname = "claude-code-ide";
    src = sources.emacs-claude-code-ide.src;
    version = "0.2.7";
    packageRequires = [
      epkgs.websocket
      epkgs.transient
      epkgs.web-server
    ];
  };
  instant-maximized-window = epkgs.melpaBuild {
    pname = "instant-maximized-window";
    src = sources.emacs-instant-maximized-window.src;
    version = "0.0.1";
  };
  nano-modeline = epkgs.melpaBuild {
    pname = "nano-modeline";
    src = sources.emacs-nano-modeline.src;
    version = "0.0.1";
  };
  typst-ts-mode = epkgs.melpaBuild {
    pname = "typst-ts-mode";
    src = sources.emacs-typst-ts-mode.src;
    version = "0.0.1";
  };
  eglot-x = epkgs.melpaBuild {
    pname = "eglot-x";
    src = sources.emacs-eglot-x.src;
    version = "0.0.1";
    packageRequires = [
      epkgs.project
      epkgs.eglot
      epkgs.xref
    ];
  };
  hide-lines = epkgs.melpaBuild {
    pname = "hide-lines";
    src = sources.emacs-hide-lines.src;
    version = "0.0.1";
  };
  org-hyperscheduler = epkgs.melpaBuild {
    pname = "org-hyperscheduler";
    src = sources.emacs-org-hyperscheduler.src;
    version = "0.0.1";
    packageRequires = [
      epkgs.websocket
      epkgs.log4e
    ];
  };
  nskk = epkgs.trivialBuild {
    pname = "nskk";
    src = sources.emacs-nskk.src;
    preBuild = ''

      cp src/*.el .

    '';
    version = "0.1.11";
  };
  # kuro = epkgs.melpaBuild {
  #   pname = "kuro";
  #   src = sources.emacs-kuro.src;
  #   version = "0.0.1";
  # };
  arto = epkgs.melpaBuild {
    pname = "arto";
    src = sources.emcas-arto.src;
    version = "0.0.1";
  };
  ghostel = epkgs.melpaBuild {
    pname = "ghostel";
    src = sources.ghostel.src;
    version = "0.38.0";
    packageRequires = [
      epkgs.compat
    ];
  };
  # Upstream projectile ships projectile-consult.el, which unconditionally
  # (require 'consult), but nixpkgs' generated package doesn't declare it
  # as a dependency, breaking byte-compilation. Add it explicitly.
  projectile = epkgs.projectile.overrideAttrs (old: {
    propagatedBuildInputs = (old.propagatedBuildInputs or []) ++ [epkgs.consult];
  });
}
