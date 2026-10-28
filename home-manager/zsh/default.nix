{pkgs, ...}: {
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    plugins = [
      {
        name = "zsh-z";
        src = "${pkgs.zsh-z}/share/zsh-z";
      }
    ];
    shellAliases = {
      c = "clear";
      python = "python3";
    };
    initContent = ''
      source ${pkgs.git}/share/git/contrib/completion/git-prompt.sh
      ${builtins.readFile ./init.zsh}
    '';
  };
}
