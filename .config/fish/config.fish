if status is-interactive
    # Commands to run in interactive sessions can go here
end

set -gx CPATH "/opt/homebrew/include"
set -gx LIBRARY_PATH "/opt/homebrew/opt/"
set -gx LDFLAGS "-L/opt/homebrew/opt/glfw"
set -gx CPPFLAGS "-L/opt/homebrew/include"

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

alias python='python3'
alias pip='pip3'

set -x PATH "$HOME/.pyenv/bin" "$PATH"
set -x PATH "/opt/homebrew/opt/llvm/bin" "$PATH"
# Setting PATH for Python 3.10
# The original version is saved in /Users/keimoriyamamoriyama/.config/fish/config.fish.pysave
# set -x PATH "/Library/Frameworks/Python.framework/Versions/3.10/bin" "$PATH"
set -x PATH "/Users/keimoriyama/Documents/hogeticlab/collectro/" "$PATH"
set -x PATH "/Users/keimoriyama/.local/bin" "$PATH"
set -x PATH "$HOME/.cargo/env" "$PATH"

set -x GITHUB_USER (git config user.name)
set -x GITHUB_ACCESS_TOKEN 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

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/keimoriyama/google-cloud-sdk/path.fish.inc' ]; . '/Users/keimoriyama/google-cloud-sdk/path.fish.inc'; end
if test -z $TMUX
  tmux new-session
end

pyenv init - | source

