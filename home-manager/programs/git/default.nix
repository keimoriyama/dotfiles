{
  programs.git = {
    enable = true;
    settings = {
      user.name = "keimoriyama";
      user.email = "kei_moriyama@icloud.com";
    };
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
}
