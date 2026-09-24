{
  emacs-skills,
  nippo,
  suiko,
  ponytail,
  isWork ? false,
  ...
}: {
  programs.agent-skills = {
    enable = true;

    # input 名での参照は extraSpecialArgs 経由の `inputs` を要求するが、
    # このリポジトリは個別の input を specialArgs に渡す方式なので path を使う。
    # idPrefix を付けると skills/emacs/<name>/SKILL.md とネストされ、
    # エージェント側の探索がトップレベルのみだと拾われない。各 source の
    # skill ID に衝突はないのでフラットに置く。
    sources.emacs = {
      path = emacs-skills;
      subdir = "skills";
    };
    # skill ディレクトリ内の docs は ../../../docs への symlink。source root を
    # .claude/skills にすると rsync --safe-links が root 外参照として落とし、
    # SKILL.md が参照するテンプレートが一つも届かない。リポジトリルートを root に
    # 取って symlink を範囲内に収める。skill 本体の位置は explicit の path で指定する。
    sources.nippo = {
      path = nippo;
      subdir = ".";
      # catalog 探索は symlink をすべてディレクトリとして辿るので、リポジトリ
      # ルートから走らせると crates/collector/assets/skill-claude.md のような
      # ファイルへの symlink で readDir が失敗する。skill 本体は explicit で
      # 直接指すため、探索はルート直下だけに絞って無効化しておく。
      filter.maxDepth = 1;
    };
    sources.suiko = {
      path = suiko;
      subdir = "skills";
    };
    sources.ponytail = {
      path = ponytail;
      subdir = "skills";
    };
    sources.personal = {
      path = ./agents/skills;
    };
    skills.enableAll = [
      "emacs"
      "personal"
      "ponytail"
      "suiko"
    ];

    # nippo は enableAll ではなく explicit で取る。同じ skill ID を両方に置くと
    # id collision で eval が落ちるため enableAll からは外してある。
    skills.explicit.nippo = {
      from = "nippo";
      # upstream は変種を 2 つ持つ。.claude/skills/nippo が Claude Code 向けで
      # allowed-tools と $ARGUMENTS を備えた日本語版、.agents/skills/nippo は
      # Codex 向けの汎用版。主に使うのは Claude Code なので前者を取る。
      path = ".claude/skills/nippo";
      # transform は SKILL.md 本文だけを差し替える。docs などの同階層ファイルは
      # symlink で保持されるのでテンプレート参照はそのまま効く。
      transform = {original, ...}:
        original
        + ''

          ## ローカル運用ルール

          - レポートの保存先は `~/Documents/org-files/reports/` に固定する。
            上記の cwd 相対の `reports/` より、このルールを優先する。
          - `ledger` は `nippo ledger --reports-dir ~/Documents/org-files/reports`
            を使う。`plan` も同じディレクトリの `nippo-*.md` と `ledger.yaml` を読む。
          - 日報を保存したら、Emacs で `nippo-org-journal-sync` を実行すると
            Org Journal に取り込める旨を伝える。自動実行はしない。

          ### 日報の構成

          日報モードでは `nippo-template.md` の見出しを次の構成に絞る。
          テンプレートの他の指示より、この構成を優先する。

          ```markdown
          # 日報 YYYY年MM月DD日（曜日）
          ## 今日の作業
          ### <プロジェクト名>（N セッション・M メッセージ）
          ### その他
          ## 統計
          ```

          - `## 今日の作業` のヘッダ項目 (作業時間帯・ソースなど) と、
            プロジェクトごとの小見出しの付け方はテンプレートのとおりにする。
          - `## 判断の記録`、`## 用語・コミュニケーションレビュー`、
            `## Unclear points`、`## 参考リンク` は書かない。
          - `Unclear points` を書かないので、生成後に `/nippo ledger` を案内しない。

          ### 補助データソース

          エージェントログだけでは手作業や思考の経過が落ちるため、以下も突き合わせる。
          いずれも Read と Glob だけで扱う（このスキルの `allowed-tools` に
          Grep や汎用 Bash は無い）。ファイルが無いのは正常なので、
          その場合は黙って飛ばす。

          - `~/Documents/org-files/journal/YYYYMMDD.org` — org-journal の日次
            ジャーナル。`meta.period.from` 〜 `meta.period.to` の各日付について
            `YYYYMMDD.org` を Read する。`** HH:MM <見出し>` 形式の見出しが
            手書きの記録で、その日に何を考えていたかを補う。
            ただし **`** Nippo` 見出し以下は読まない。**
            これは `nippo-org-journal-sync` が過去の日報を取り込んだもので、
            自分の出力を入力として読み直す循環になる。
          - `~/Documents/org-files/projects/*.org` — プロジェクトノート。Glob で
            列挙して Read し、`CLOSED: [YYYY-MM-DD ...]` の日付が対象期間に
            入っている `DONE` 見出しだけを拾う。期間外の DONE は対象外。

          これらはエージェントログを置き換えるものではなく、補助に留める。
          `stats` の数値は収集 JSON をそのまま使い、ここで拾った件数を
          足し込んだり作業時間帯を引き直したりしない。
          ジャーナルや DONE 由来の記述は、ログから書いた内容の裏付けや
          背景説明として本文に織り込む。
        '';
    };

    targets.claude.enable = true;
    # codex 本体を入れない業務用マシンでは skill の配置も不要。
    targets.codex.enable = !isWork;
  };
}
