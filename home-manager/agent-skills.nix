{inputs, ...}: let
  inherit (inputs) emacs-skills suiko ponytail;
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
    targets.claude.enable = true;
  };
}
