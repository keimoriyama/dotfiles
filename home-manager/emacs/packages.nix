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
  agent-shell-notifications = epkgs.melpaBuild {
    pname = "agent-shell-notifications";
    src = sources.emacs-agent-shell-notifications.src;
    version = "0.1.0";
    # The knockknock provider requires the knockknock package, which isn't
    # packaged in nixpkgs, so byte-compiling it would fail. We use a custom
    # macOS provider anyway (libnotify needs D-Bus, absent on darwin), so drop
    # it along with the test file rather than disabling error checking wholesale.
    preBuild = ''
      rm -f agent-shell-notifications-knockknock.el agent-shell-notifications-tests.el
    '';
    packageRequires = [epkgs.agent-shell];
  };
  agent-shell-attention = epkgs.melpaBuild {
    pname = "agent-shell-attention";
    src = sources.emacs-agent-shell-attention.src;
    version = "0.0.2";
    packageRequires = [epkgs.agent-shell];
  };
  agent-shell-tramp = epkgs.melpaBuild {
    pname = "agent-shell-tramp";
    src = sources.emacs-agent-shell-tramp.src;
    version = "0.2.0";
    preBuild = ''
      rm -f agent-shell-tramp-tests.el
    '';
    packageRequires = [
      epkgs.agent-shell
      epkgs.acp
    ];
  };
  agent-shell-dashboard = epkgs.melpaBuild {
    pname = "agent-shell-dashboard";
    src = sources.emacs-agent-shell-dashboard.src;
    version = "0.1.0";
    packageRequires = [
      epkgs.agent-shell
      # Optional at runtime — (require 'modus-themes nil t) — but needed so
      # byte-compilation resolves its faces. projectile stays out: it's only
      # reached through fboundp guards, and our projectile is the override below.
      epkgs.modus-themes
    ];
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
  copy-file-on-save = epkgs.melpaBuild {
    pname = "copy-file-on-save";
    src = sources.emacs-auto-deployment.src;
    version = "0.1.0";
  };
  # Upstream projectile ships projectile-consult.el, which unconditionally
  # (require 'consult), but nixpkgs' generated package doesn't declare it
  # as a dependency, breaking byte-compilation. Add it explicitly.
  projectile = epkgs.projectile.overrideAttrs (old: {
    propagatedBuildInputs = (old.propagatedBuildInputs or []) ++ [epkgs.consult];
  });
}
