{
  pkgs,
  org-babel,
  sources,
}: let
  # Tangle the emacs-lisp blocks out of an .org config into a plain init string.
  tangle = org-babel.lib.tangleOrgBabel {languages = ["emacs-lisp"];};
  tangleOrg = org: tangle (builtins.readFile org);

  emacsPkgs = pkgs.emacsWithPackagesFromUsePackage {
    package = pkgs.emacs;
    config = builtins.toFile "empty.el" "";
    extraEmacsPackages = import ./epkgs.nix {inherit pkgs sources;};
  };

  # emacs-twist/org-babel は別ファイルへの :tangle を扱えないので、
  # yasnippet.org はバッチ Emacs の org-babel-tangle-file でタグルする。
  # :tangle が "~/.emacs.d/snippets/..." へ展開されるよう HOME を
  # ビルドディレクトリに向け、生成されたツリーを丸ごと回収する。
  yasnippetSnippets =
    pkgs.runCommand "yasnippet-snippets" {
      nativeBuildInputs = [pkgs.emacs-nox];
    } ''
      export HOME="$PWD"
      cp ${./yasnippet.org} yasnippet.org
      emacs --batch -Q \
        --eval '(require (quote ob-tangle))' \
        --eval '(org-babel-tangle-file "yasnippet.org")'
      mv -- "$HOME/.emacs.d/snippets" "$out"
    '';
in {
  programs.emacs = {
    enable = true;
    package = emacsPkgs;
  };
  home = {
    file = {
      ".emacs.d/init.el".text = tangleOrg ./init.org;
      ".emacs.d/early-init.el".text = tangleOrg ./early-init.org;
      ".emacs.d/misc/yasnippet.org".source = ./yasnippet.org;
      # recursive=true で実ディレクトリ + ファイルごとのシンボリンクにし、
      # 既存の ~/.emacs.d/snippets との衝突や yasnippet 側からの
      # 書き込み（新規スニペット等）と共存できるようにする
      ".emacs.d/snippets" = {
        source = yasnippetSnippets;
        recursive = true;
      };
    };
    packages = with pkgs; [
      emacs-lsp-booster
    ];
  };
}
