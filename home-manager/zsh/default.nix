{pkgs, ...}: {
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    syntaxHighlighting = {
      enable = true;
      highlighters = ["brackets"];
      # GitHub 配色の zsh-syntax-highlighting テーマは見当たらないので、
      # Emacs の github テーマと同じ色を割り当てる。
      styles = {
        command = "fg=#990000";
        builtin = "fg=#990000";
        function = "fg=#990000";
        alias = "fg=#990000";
        precommand = "fg=#990000,underline";
        reserved-word = "bold";
        unknown-token = "fg=#dd1144,bold";
        commandseparator = "fg=#445588";
        redirection = "fg=#445588";
        path = "underline";
        globbing = "fg=#009926";
        single-hyphen-option = "fg=#0086b3";
        double-hyphen-option = "fg=#0086b3";
        single-quoted-argument = "fg=#dd1144";
        double-quoted-argument = "fg=#dd1144";
        dollar-quoted-argument = "fg=#dd1144";
        back-quoted-argument = "fg=#990073";
        dollar-double-quoted-argument = "fg=#990073";
        assign = "fg=#0086b3";
        comment = "fg=#999988,italic";
      };
    };
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
