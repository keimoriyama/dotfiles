{
  programs.git = {
    enable = true;
    userName = "keimoriyama";
    userEmail = "kei_moriyama@icloud.com";
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
