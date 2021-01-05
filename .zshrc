## 色を使用出来るようにする
autoload -Uz colors
colors
autoload -Uz compinit && compinit
compinit
setopt auto_list
setopt auto_menu
zstyle ':completion:*:default' menu select=1
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
setopt auto_cd
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' '+m:{[:upper:]}={[:lower:]}'

## タブ補完時に大文字小文字を区別しない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

export ZPLUG_HOME=~/.zplug

if [[ ! -d ~/.zplug ]];then
  git clone https://github.com/zplug/zplug.git ~/.zplug
fi

source ~/.zplug/init.zsh
zplug 'zplug/zplug', hook-build:'zplug --self-manage'
zplug "mollifier/cd-gitroot"
zplug "hchbaw/opp.zsh", hook-build:"__zsh_version 5.0.8"
zplug "zsh-users/zsh-history-substring-search", hook-build:"__zsh_version 4.3"
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "mafredri/zsh-async", from:"github", use:"async.zsh"
zplug "sindresorhus/pure"
zplug "zsh-users/zsh-completions"
zplug "chrissicool/zsh-256color"
zplug "peterhurford/git-aliases.zsh"
# ヒストリの補完を強化する
zplug "zsh-users/zsh-history-substring-search", defer:3

if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load

autoload -U promptinit; promptinit
PURE_CMD_MAX_EXEC_TIME=10

# change the path color
zstyle :prompt:pure:path color white

# change the color for both `prompt:success` and `prompt:error`
zstyle ':prompt:pure:prompt:*' color cyan
zstyle ':prompt:pure:path' color blue

# turn on git stash status
zstyle :prompt:pure:git:stash show yes

setopt no_beep

alias g='git'
alias ga='git add'
alias gd='git diff'
alias gs='git status'
alias gp='git push'
alias gpo='git push origin'
alias gb='git branch'
alias gst='git status'
alias gco='git checkout'
alias gf='git fetch'
alias gc='git commit'

alias python='python3'
alias pip='pip3'

alias c='clear'

if [[ ! -n $TMUX ]]; then
  tmux new-session
fi

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/kei/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/kei/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/kei/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/kei/Downloads/google-cloud-sdk/completion.zsh.inc'; fi
