if status is-interactive
    # Commands to run in interactive sessions can go here
end
set PATH /usr/local/opt/llvm/bin $PATH 
set PATH ~/.config/coc/extensions/coc-clangd-data/install/12.0.0/clangd_12.0.0/bin $PATH
set PATH ~/.nodebrew/current/bin $PATH
set PATH ~/bin/digdag $PATH

set -gx LDFLAGS "-L/opt/homebrew/opt/llvm/lib"
set -gx CPPFLAGS "-I/opt/homebrew/opt/llvm/include"

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

alias c='clear'
alias vim='nvim'

alias gcc='gcc-11'
alias g++='g++-11'

if test -z $TMUX
  tmux new-session
end


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /opt/homebrew/Caskroom/miniforge/base/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

fish_add_path /opt/homebrew/opt/llvm/bin
fish_add_path /opt/homebrew/opt/ruby/bin
