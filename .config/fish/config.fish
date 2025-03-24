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
alias n='nvim'
alias python='python3'

set -gx MOCWORD_DATA $HOME/.local/mocword-data/mocword.sqlite

set -gx DPP_PATH $HOME/.cache/dpp

set -gx HYDRA_FULL_ERROR 1


#view
set -g theme_display_date yes
set -g theme_date_format "+%F %H:%M"
set -g theme_display_git_default_branch yes
set -g theme_color_scheme dark
set fish_plugins theme peco

set -x PATH "/nix/var/nix/profiles/default/bin/" "$PATH"
set -x PATH "$HOME/.nix-profile/bin" "$PATH"
