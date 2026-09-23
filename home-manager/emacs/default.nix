{
  pkgs,
  org-babel,
  sources,
}: let
  # Tangle the emacs-lisp blocks out of an .org config into a plain init string.
  tangle = org-babel.lib.tangleOrgBabel {languages = ["emacs-lisp"];};
  tangleOrg = org: tangle (builtins.readFile org);

  emacsPkgs = import ./emacs-with-packages.nix {inherit pkgs sources;};

  # emacsWithPackagesFromUsePackage が生成する Emacs.app は、Contents/MacOS/Emacs が
  # 別バンドル (素の emacs) の実行ファイルを exec するラッパーになっている。
  # LaunchServices は起動したバンドルへプロセスを紐付けられなくなり、
  # NSWorkspace の processIdentifier が -1 を返すため、アクセシビリティ API で
  # ウィンドウを管理するツール (AeroSpace など) から Emacs が見えなくなる。
  # 実体のバイナリをバンドル内に置き、ラッパーが設定していた環境変数は
  # Info.plist の LSEnvironment 経由で渡すことで exec を無くす。
  emacsWithLaunchServicesApp =
    pkgs.runCommand "${emacsPkgs.name}-app" {
      nativeBuildInputs = [pkgs.python3];
    } ''
            mkdir -p "$out/Applications"
            for entry in ${emacsPkgs}/*; do
              name="$(basename "$entry")"
              if [ "$name" != Applications ]; then
                ln -s "$entry" "$out/$name"
              fi
            done

            app="$out/Applications/Emacs.app"
            cp -R ${emacsPkgs}/Applications/Emacs.app "$app"
            chmod -R u+w "$app"
            rm "$app/Contents/MacOS/Emacs" "$app/Contents/MacOS/.Emacs-wrapped"
            cp ${pkgs.emacs}/Applications/Emacs.app/Contents/MacOS/Emacs "$app/Contents/MacOS/Emacs"
            chmod u+w "$app/Contents/MacOS/Emacs"

            EMACS_LOAD_PATH="$(${emacsPkgs}/bin/emacs -Q --batch \
              --eval '(princ (or (getenv "EMACSLOADPATH") ""))')" \
            EMACS_NATIVE_LOAD_PATH="$(${emacsPkgs}/bin/emacs -Q --batch \
              --eval '(princ (or (getenv "EMACSNATIVELOADPATH") ""))')" \
            python3 -c '
      import os, plistlib, sys

      with open(sys.argv[1], "rb") as f:
          info = plistlib.load(f)
      info["LSEnvironment"] = {
          "EMACSLOADPATH": os.environ["EMACS_LOAD_PATH"],
          "EMACSNATIVELOADPATH": os.environ["EMACS_NATIVE_LOAD_PATH"],
      }
      with open(sys.argv[1], "wb") as f:
          plistlib.dump(info, f)
      ' "$app/Contents/Info.plist"
    '';

  emacsPackage =
    if pkgs.stdenv.hostPlatform.isDarwin
    then emacsWithLaunchServicesApp
    else emacsPkgs;

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
  home = {
    file = {
      ".emacs.d/init.el".text = tangleOrg ./init.org;
      ".emacs.d/early-init.el".text = tangleOrg ./early-init.org;
      ".emacs.d/lisp/agent-shell-provider-usage.el".source = ./agent-shell-provider-usage.el;
      ".emacs.d/lisp/agent-shell-session-name.el".source = ./agent-shell-session-name.el;
      ".emacs.d/lisp/agent-usage-format.el".source = ./agent-usage-format.el;
      ".emacs.d/lisp/claude-usage.el".source = ./claude-usage.el;
      ".emacs.d/lisp/nippo-org-journal.el".source = ./nippo-org-journal.el;
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
      emacsPackage
      emacs-lsp-booster
    ];
  };
}
