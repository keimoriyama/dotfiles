# AI エージェントが org-files を任意のディレクトリから読むための CLI。
{pkgs, ...}: {
  home.packages = [
    (pkgs.writeShellApplication {
      name = "org-files";
      runtimeInputs = with pkgs; [ripgrep fd coreutils];
      text = ''
        root="''${ORG_FILES_ROOT:-$HOME/ghq/github.com/keimoriyama/org-files}"
        cmd="''${1:-}"
        [ "$#" -gt 0 ] && shift

        case "$cmd" in
        root)
          echo "$root"
          ;;
        list)
          # 引数は fd にそのまま渡す (例: org-files list -e org tasks)
          fd --type f --extension org "$@" . "$root"
          ;;
        search)
          # 引数は rg にそのまま渡す (例: org-files search -i 'TODO.*foo')
          rg --glob '*.org' "$@" "$root"
          ;;
        cat)
          # root からの相対パスを受け取る
          cat "$root/$1"
          ;;
        *)
          echo "usage: org-files {root|list|search|cat} [args...]" >&2
          exit 2
          ;;
        esac
      '';
    })
  ];
}
