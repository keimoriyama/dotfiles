# zsh と TRAMP の /bin/sh で共有するため、POSIX sh の構文だけを使う。
if [ -r "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ]; then
  . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
fi

# API キーなどリポジトリに入れない環境変数はこの管理外ファイルに書く。
# TRAMP の /bin/sh からも読むため、内容は POSIX sh 互換にする。
zsh_config_home=${XDG_CONFIG_HOME:-"$HOME/.config"}
if [ -r "$zsh_config_home/zsh/secrets.zsh" ]; then
  . "$zsh_config_home/zsh/secrets.zsh"
fi
unset zsh_config_home

path_prepend() {
  [ -d "$1" ] || return
  case ":$PATH:" in
    *:"$1":*) ;;
    *) PATH="$1${PATH:+:$PATH}" ;;
  esac
}

path_append() {
  [ -d "$1" ] || return
  case ":$PATH:" in
    *:"$1":*) ;;
    *) PATH="${PATH:+$PATH:}$1" ;;
  esac
}

# 先頭へ追加する順序を保つため、逆順に処理する。
path_prepend /nix/var/nix/profiles/default/bin
path_prepend "/etc/profiles/per-user/$USER/bin"
path_prepend "$HOME/.nix-profile/bin"

path_append /opt/homebrew/bin
path_append "$HOME/.cargo/bin"
path_append "$HOME/.local/bin"
path_append "$HOME/.roswell/bin"

export PATH
export SHELL="/etc/profiles/per-user/$USER/bin/zsh"
unset -f path_prepend path_append
