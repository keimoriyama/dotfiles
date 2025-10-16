{pkgs}: {
  home = {
    file = {
      ".config/nushell/config.nu".source = ./config.nu;
    };
    packages = [pkgs.nushell];
  };
  programs.nushell = {
    enable = true;
  };
}
