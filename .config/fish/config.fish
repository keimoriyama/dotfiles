if status is-interactive
    # Commands to run in interactive sessions can go here
end

alias vim='nvim'
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

alias rm='trash-put'

alias ls='lsd -G'
alias c='clear'
alias g++='g++-13'
alias vim='nvim'
alias python='python3'

fish_add_path $HOME/.pyenv/bin
fish_add_path /opt/homebrew/opt/llvm/bin
fish_add_path /Users/keimoriyama/.local/bin
fish_add_path /Users/keimoriyama/Applications
fish_add_path /opt/homebrew/bin

fish_add_path $HOME/.local/share/nvim/mason/bin
fish_add_path $HOME/.cargo/bin
fish_add_path /opt/homebrew/opt/llvm/bin

set -gx PYENV_ROOT $HOME/.pyenv
set -gx  LDFLAGS /opt/homebrew/opt/openssl@3/lib
set -gx  CPPFLAGS /opt/homebrew/opt/openssl@3/include

set -gx LDFLAGS /opt/homebrew/opt/llvm/lib
set -gx CPPFLAGS /opt/homebrew/opt/llvm/include

set -gx NEOVIM_HOME $HOME/.local/nvim

fish_add_path $HOME/.local/nvim/bin

set -gx PKG_CONFIG_PATH /opt/homebrew/opt/libpq/lib/pkgconfig

set -gx GITHUB_USER keimoriyama
set -gx GITHUB_ACCESS_TOKEN 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

set -gx OPENAI_API_KEY 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
set -gx OPENAI_ORG_KEY org-72Zir3ce8563QX7AVgOysMF6


