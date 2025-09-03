{
  epkgs,
  sources,
}: {
  copilot = epkgs.melpaBuild {
    pname = "copilot";
    src = sources.emacs-copilot.src;
  };
}
