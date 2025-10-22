{
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
    # files =''("*[^0-9]*.el")'';
    patches = [./yatexlib.diff];
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
  bimove = epkgs.melpaBuild{
  pname = "bimove";
  src = sources.emacs-bimove.src;
  version="0.0.1"
  };
}
