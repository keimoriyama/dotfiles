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
