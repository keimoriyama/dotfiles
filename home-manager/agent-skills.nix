{
  emacs-skills,
  nippo,
  suiko,
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
    sources.nippo = {
      path = nippo;
      subdir = ".agents/skills";
    };
    sources.suiko = {
      path = suiko;
      subdir = "skills";
    };
    sources.personal = {
      path = ./agents/skills;
    };
    skills.enableAll = [
      "emacs"
      "nippo"
      "personal"
      "suiko"
    ];

    targets.claude.enable = true;
    # codex 本体を入れない業務用マシンでは skill の配置も不要。
    targets.codex.enable = !isWork;
  };
}
