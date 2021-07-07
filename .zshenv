[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_OPTS='--height 40% --reverse --border'
export FZF_CTRL_T_COMMAND='rg --files --hidden --follow --glob "!.git/*"'


export PATH=$HOME/.cargo/bin:$PATH
#eval "$(pyenv virtualenv-init -)"
export PYENV_VIRTUALENV_DISABLE_PROMPT=1

export TERM=xterm-256color
export PATH="/usr/local/opt/llvm/bin:$PATH"
export PATH=~/anaconda3/bin:$PATH
export PATH=$PATH:~/.config/coc/extensions/coc-clangd-data/install/12.0.0/clangd_12.0.0/bin
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
alias vim='nvim'

