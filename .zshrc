# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

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

alias ls='lsd -G'
alias c='clear'
alias g++='g++-13'
alias vim='nvim'

chpwd() {
	if [[ $(pwd) != $HOME ]]; then;
		ls
	fi
}
# コマンド補完についての設定
autoload -Uz compinit
compinit
zstyle ':completion:*' menu select
setopt correct

eval "$(pyenv init --path)" # これを追記
eval "$(pyenv init -)"
eval "$(mutagen daemon start)"

if type brew &>/dev/null; then
	FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
	source $(brew --prefix)/opt/zsh-git-prompt/zshrc.sh
	source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
	source $(brew --prefix)/opt/z/etc/profile.d/z.sh
	autoload -Uz compinit
	compinit
fi
source ~/powerlevel10k/powerlevel10k.zsh-theme
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# >>> juliaup initialize >>>

# !! Contents within this block are managed by juliaup !!

path=('/Users/keimoriyama/.juliaup/bin' $path)
export PATH

# <<< juliaup initialize <<<
