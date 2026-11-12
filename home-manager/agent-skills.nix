{inputs, ...}: let
  inherit (inputs) emacs-skills suiko;
in {
  programs.agent-skills = {
    enable = true;

    # idPrefix を付けると skills/emacs/<name>/SKILL.md とネストされ、
    # エージェント側の探索がトップレベルのみだと拾われない。各 source の
    # skill ID に衝突はないのでフラットに置く。
    sources.emacs = {
      path = emacs-skills;
      subdir = "skills";
    };
    sources.suiko = {
      path = suiko;
      subdir = "skills";
    };
    sources.personal = {
      path = ./agents/skills;
    };
    # emacs-skills は実際に使う skill だけを入れる。
    skills.enable = ["emacsclient"];
    skills.enableAll = [
      "personal"
      "suiko"
    ];
    targets.claude.enable = true;
  };
}
