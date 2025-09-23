{pkgs}: {
  home = {
    file = {
      ".textlintrc.json".source = ./.textlintrc.json;
    };
    packages = with pkgs; [
      textlint
      textlint-plugin-org
      textlint-plugin-latex2e
      textlint-rule-preset-ja-technical-writing
    ];
  };
}
