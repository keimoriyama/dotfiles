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
	patches = [./yatex.diff];
  };
}
