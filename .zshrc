autoload -Uz colors && colors
zstyle ":completion:*:commands" rehash 1
# 小文字でも大文字ディレクトリ、ファイルを補完できるようにする
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

setopt no_beep

alias g='git'
alias ga='git add'
alias gd='git diff'
alias gp='git push'
alias gpo='git push origin'
alias gb='git branch'
alias gs='git status'
alias gco='git checkout'
alias gf='git fetch'
alias gc='git commit'
alias gt='git log --graph --pretty=format:'\''%x09%C(auto) %h %Cgreen %ar %Creset%x09by\"%C(cyan ul)%an%Creset\" %x09%C(auto)%s %d'\'''

alias c='clear'
alias g++='g++-13'

chpwd() {
	if [[ $(pwd) != $HOME ]]; then;
		ls
	fi
}

