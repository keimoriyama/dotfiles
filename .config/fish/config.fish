if status is-interactive
    # Commands to run in interactive sessions can go here
end
set PATH /usr/local/opt/llvm/bin $PATH 
set PATH ~/.config/coc/extensions/coc-clangd-data/install/12.0.0/clangd_12.0.0/bin $PATH
set PATH ~/.nodebrew/current/bin $PATH

eval (/opt/homebrew/bin/brew shellenv)

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

if test -z $TMUX
  tmux new-session
end


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /opt/homebrew/Caskroom/miniforge/base/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

