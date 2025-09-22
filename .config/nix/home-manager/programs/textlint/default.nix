{pkgs}: {
  home = {
    file = {
      ".textlintrc".source = ./.textlintrc;
    };
    packages = with pkgs; [
      textlint
      textlint-plugin-org
      textlint-plugin-latex2e
      textlint-rule-preset-ja-technical-writing
      ];
  };
}
