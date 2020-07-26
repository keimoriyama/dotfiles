[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_OPTS='--height 40% --reverse --border'
export FZF_CTRL_T_COMMAND='rg --files --hidden --follow --glob "!.git/*"'

export PYENV_ROOT=/usr/local/var/pyenv
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi


export JAVA_HOME=`/usr/libexec/java_home -v 14`
export PATH=${JAVA_HOME}/bin:${PATH}

export PATH=$HOME/.nodebrew/current/bin:$PATH
export PATH=$HOME/.cargo/bin:$PATH
#eval "$(pyenv virtualenv-init -)"
export PYENV_VIRTUALENV_DISABLE_PROMPT=1

export TERM=xterm-256color
export PATH="/usr/local/opt/llvm/bin:$PATH"
