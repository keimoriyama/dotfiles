typeset -U path
# fish_add_path と同じく、存在しないディレクトリは追加しない。
path=({"$HOME/.nix-profile",/etc/profiles/per-user/$USER,/nix/var/nix/profiles/default}/bin(N-/) $path)
path+=({/opt/homebrew,"$HOME/.cargo","$HOME/.local","$HOME/.roswell"}/bin(N-/))
export PATH
export SHELL="/etc/profiles/per-user/$USER/bin/zsh"

[[ -f "$MOCWORD_DATA" ]] || {
  mkdir -p "${MOCWORD_DATA:h}"
  curl -L -o "$MOCWORD_DATA.gz" https://github.com/high-moctane/mocword-data/releases/download/eng20200217/mocword.sqlite.gz && gzip -d "$MOCWORD_DATA.gz"
}

autoload -Uz add-zsh-hook chpwd_recent_dirs chpwd_recent_filehandler
zstyle ':chpwd:*' recent-dirs-max 500
zstyle ':chpwd:*' recent-dirs-file "${XDG_STATE_HOME:-$HOME/.local/state}/zsh/chpwd-recent-dirs"
mkdir -p "${XDG_STATE_HOME:-$HOME/.local/state}/zsh"
add-zsh-hook chpwd chpwd_recent_dirs

# 大文字小文字を無視し、それでも候補がなければ区切り文字単位・部分一致の順に広げる。
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

function _autols { ls }
add-zsh-hook chpwd _autols

function _fzf_dir { FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS $FZF_ALT_C_OPTS" fzf }
function gcd { local root=$(git rev-parse --show-toplevel) && [[ -n $root ]] && builtin cd -- "$root" }
function cdf {
  local -a reply
  chpwd_recent_filehandler
  local dir=$(print -rl -- ${^reply}(N-/) | _fzf_dir) && [[ -n $dir ]] && cd -- "$dir"
}
function bd {
  local dir=$PWD
  local -a parents
  repeat 20; do
    dir=${dir:h}
    parents+=$dir
    [[ $dir == / ]] && break
  done
  dir=$(print -rl -- $parents | _fzf_dir) && [[ -n $dir ]] && cd -- "$dir"
}
function _ghq_fzf {
  local dir=$(ghq list -p | _fzf_dir)
  if [[ -n $dir ]]; then
    BUFFER=" cd ${(q)dir}"
    zle accept-line
  else
    zle reset-prompt
  fi
}
zle -N _ghq_fzf
bindkey '^G' _ghq_fzf

# opencode.json の kadode プロバイダーは {env:KADODE_TOKEN} を読む。
function opencode {
  local token
  token=$(kadode refresh) || {
    print -u2 "kadode のログインが必要です: kadode login ..."
    return 1
  }
  KADODE_TOKEN=$token command opencode "$@"
}

# fish の prompt_pwd と同じく、最後以外のディレクトリを1文字に縮める。
function _prompt_pwd {
  local -a parts=("${(@s:/:)${(%):-%~}}")
  local out= part
  for part in "${(@)parts[1,-2]}"; do
    [[ $part == .* ]] && out+=${part[1,2]}/ || out+=${part[1]}/
  done
  out+=${parts[-1]}
  print -rn -- "${out//\%/%%}"
}

# fish の既定のタイトルと同じく、省略パスと実行中のコマンド名をターミナルのタイトルに出す。
function _title_precmd { print -Pn "\e]0;$(_prompt_pwd)\a" }
function _title_preexec { print -Pn "\e]0;${${1%% *}//\%/%%} $(_prompt_pwd)\a" }
add-zsh-hook precmd _title_precmd
add-zsh-hook preexec _title_preexec

GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM=auto
setopt PROMPT_SUBST
PROMPT='%F{cyan}$(_prompt_pwd) %F{red}❯%F{yellow}❯%F{green}❯%f '
RPROMPT='$(__git_ps1 " (%%F{yellow}%s%%f)")'
