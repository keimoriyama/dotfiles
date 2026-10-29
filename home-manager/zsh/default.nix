{pkgs, ...}: {
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    syntaxHighlighting = {
      enable = true;
      highlighters = ["brackets"];
      # 既定では引数やクォート文字列が無色なので、fish に近い配色にする。
      styles = {
        command = "fg=blue";
        builtin = "fg=blue";
        function = "fg=blue";
        alias = "fg=blue";
        precommand = "fg=blue,underline";
        reserved-word = "fg=magenta";
        unknown-token = "fg=red,bold";
        commandseparator = "fg=cyan";
        redirection = "fg=cyan";
        path = "fg=cyan,underline";
        globbing = "fg=yellow";
        single-hyphen-option = "fg=cyan";
        double-hyphen-option = "fg=cyan";
        single-quoted-argument = "fg=yellow";
        double-quoted-argument = "fg=yellow";
        dollar-quoted-argument = "fg=yellow";
        back-quoted-argument = "fg=magenta";
        dollar-double-quoted-argument = "fg=magenta";
        assign = "fg=magenta";
        comment = "fg=8";
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
