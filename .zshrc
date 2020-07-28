autoload -Uz compinit && compinit
setopt auto_list
setopt auto_menu
zstyle ':completion:*:default' menu select=1
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
setopt auto_cd

export ZPLUG_HOME=~/.zplug

if [[ ! -d ~/.zplug ]];then
  git clone https://github.com/zplug/zplug.git ~/.zplug
fi

source ~/.zplug/init.zsh
zplug 'zplug/zplug', hook-build:'zplug --self-manage'
zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme, as:theme
zplug "mollifier/cd-gitroot"
zplug "hchbaw/opp.zsh", hook-build:"__zsh_version 5.0.8"
zplug "zsh-users/zsh-history-substring-search", hook-build:"__zsh_version 4.3"
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "zsh-users/zsh-completions"
zplug "chrissicool/zsh-256color"
zplug "peterhurford/git-aliases.zsh"
# 入力途中に候補をうっすら表示
zplug "zsh-users/zsh-autosuggestions"
# ヒストリの補完を強化する
zplug "zsh-users/zsh-history-substring-search", defer:3

if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir newline vcs)
POWERLEVEL9K_MODE='awesome-patched'

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

alias vim='nvim'

alias c='clear'

alias sl='sl -aF'

setopt auto_cd


if [[ ! -n $TMUX ]]; then
  tmux new-session
fi
