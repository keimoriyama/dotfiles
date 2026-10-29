{pkgs, ...}: {
  # ls と補完候補の色。GitHub 配色に合わせるため 24bit 指定にしている。
  home.sessionVariables.LS_COLORS = builtins.concatStringsSep ":" [
    "di=01;38;2;68;85;136"
    "ln=38;2;153;0;115"
    "or=38;2;221;17;68"
    "ex=38;2;153;0;0"
    "so=38;2;0;153;38"
    "pi=38;2;0;153;38"
    "bd=38;2;0;134;179"
    "cd=38;2;0;134;179"
  ];
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
        path = "fg=#445588,underline";
        autodirectory = "fg=#445588,underline";
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
      # macOS の ls は LS_COLORS を読まないので GNU ls を使う。
      alias ls='${pkgs.coreutils}/bin/ls --color=auto'
      source ${pkgs.git}/share/git/contrib/completion/git-prompt.sh
      ${builtins.readFile ./init.zsh}
    '';
  };
}
