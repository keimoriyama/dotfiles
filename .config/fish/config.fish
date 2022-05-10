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

alias gcc='gcc-11'
alias g++='g++-11'

alias vim='nvim'

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /opt/homebrew/Caskroom/miniforge/base/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

fish_add_path /opt/homebrew/opt/llvm/bin
fish_add_path /opt/homebrew/opt/ruby/bin

set -x GOPRIVATE 'github.com/hogeticlab'

# Setting PATH for Python 3.10
# The original version is saved in /Users/kei/.config/fish/config.fish.pysave
set -x PATH "/Library/Frameworks/Python.framework/Versions/3.10/bin" "$PATH"
set -x PATH "/Users/kei/Documents/hogeticlab/collectro/" "$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/kei/google-cloud-sdk/path.fish.inc' ]; . '/Users/kei/google-cloud-sdk/path.fish.inc'; end

set -x GITHUB_USER (git config user.name)
set -x GITHUB_ACCESS_TOKEN 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
# tmux の自動起動
if test -z $TMUX
	  tmux new-session
	  end
