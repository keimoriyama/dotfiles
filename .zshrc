autoload -Uz colors && colors
zstyle ":completion:*:commands" rehash 1

export PATH=/Users/keimoriyama/Documents/hogeticlab/collectro/:$PATH
export GITHUB_USER=keimoriyama
export GITHUB_ACCESS_TOKEN=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

export LDFLAGS=-L/opt/homebrew/opt/llvm/lib
export CPPFLAGS=-I/opt/homebrew/opt/llvm/include

export PATH=$HOME/.pyenv/bin:$PATH
export PATH=/opt/homebrew/opt/llvm/bin:$PATH
export PATH=/Users/keimoriyama/.local/bin:$PATH

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

alias c='clear'

alias vim='nvim'

alias python='python3'
alias pip='pip3'

git_prompt() {
  if [ "$(git rev-parse --is-inside-work-tree 2> /dev/null)" = true ]; then
    PROMPT='%F{034}%n%f %F{081}%~%f $(git_super_status)'
    PROMPT+=""$'\n'"%# "
  else
    PROMPT="%F{034}%n%f %F{081}%~%f "$'\n'"%# "
  fi
}

precmd(){
	git_prompt
}

# コマンド補完についての設定
autoload -Uz compinit
compinit
zstyle ':completion:*' menu select
setopt correct

if type brew &>/dev/null; then
	FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
	source /opt/homebrew/opt/zsh-git-prompt/zshrc.sh
	source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
	autoload -Uz compinit
	compinit
fi
