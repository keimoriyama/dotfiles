{emacs-skills, ...}: {
  programs.agent-skills = {
    enable = true;

    # input 名での参照は extraSpecialArgs 経由の `inputs` を要求するが、
    # このリポジトリは個別の input を specialArgs に渡す方式なので path を使う。
    # idPrefix を付けると skills/emacs/<name>/SKILL.md とネストされ、
    # エージェント側の探索がトップレベルのみだと拾われない。source が
    # 1 つで ID 衝突もないのでフラットに置く。
    sources.emacs = {
      path = emacs-skills;
      subdir = "skills";
    };
    skills.enableAll = ["emacs"];

    targets.claude.enable = true;
    targets.codex.enable = true;
  };
}
