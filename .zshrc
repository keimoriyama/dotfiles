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

if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir newline vcs)
POWERLEVEL9K_MODE='awesome-patched'

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

setopt auto_cd


if [[ ! -n $TMUX ]]; then
  tmux new-session
fi
