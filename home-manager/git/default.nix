{pkgs, ...}: let
  # キーを辞書順に、配列を要素順にそろえる。jq . だけでは入力の順序を保つため
  # 並び替えだけの差分が消えない。クォートの入れ子を避けてファイルから読ませる。
  jsonsort = pkgs.writeText "jsonsort.jq" ''walk(if type == "array" then sort else . end)'';
in {
  programs.git = {
    enable = true;
    settings = {
      user.name = "keimoriyama";
      user.email = "keimoriy4ma@gmail.com";
      # Claude Code などの外部ツールが settings.json を書き戻すと、内容が同じでも
      # キー順や配列順が変わる。index 側を正規化して実際の変更だけを差分に出す。
      # git のレイヤーなので lazygit などの diff ビューアにもそのまま効く。
      filter.jsonsort.clean = "${pkgs.jq}/bin/jq -S -f ${jsonsort}";
    };
    # 自分の dotfiles のパスだけに絞る。グローバル設定なので広いパターンにすると
    # 他人のリポジトリの JSON まで add 時に整形してしまう。
    attributes = [
      "home-manager/claude-code/settings.json filter=jsonsort"
    ];
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

      # AI エージェントの作業記録置き場（計画書・進捗など）
      "z-ai/"
    ];
  };
}
