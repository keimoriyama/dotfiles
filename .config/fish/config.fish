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
alias vim='nvim'
alias python='python3'

fish_add_path $HOME/Library/Python/3.11/bin
fish_add_path $HOME/.cargo/bin
fish_add_path $HOME/.local/nvim/bin
fish_add_path $HOME/.local/share/aquaproj-aqua/bin


switch (uname)
    case Linux
		fish_add_path /home/kei/.deno/bin
		fish_add_path $HOME/.cargo/bin
    case Darwin
		fish_add_path /opt/homebrew/opt/llvm/bin 
		fish_add_path /Users/keimoriyama/.local/bin
		fish_add_path /Users/keimoriyama/Applications
		fish_add_path /opt/homebrew/bin
		fish_add_path /opt/homebrew/opt/llvm/bin
		alias gcc='gcc-14'
		set -gx LD_LIBRARY_PATH /opt/homebrew/Cellar/libgccjit/14.2.0/lib/gcc/14
		set -gx LDFLAGS /opt/homebrew/opt/openssl@3/lib
		set -gx CPPFLAGS /opt/homebrew/opt/openssl@3/include
		set -gx PKG_CONFIG_PATH /opt/homebrew/opt/libpq/lib/pkgconfig
    case FreeBSD NetBSD DragonFly
            echo Hi Beastie!
    case '*'
            echo Hi, stranger!
end

set -gx MOCWORD_DATA $HOME/.local/mocword-data/mocword.sqlite

set -gx DPP_PATH $HOME/.cache/dpp

set -gx PYENV_ROOT $HOME/.pyenv

set -gx NEOVIM_HOME $HOME/.local/nvim
set -gx AQUA_GLOBAL_CONFIG $HOME/.dotfiles/aqua/aqua.yaml

set -gx HYDRA_FULL_ERROR 1

function attach_tmux_session_if_needed
    set ID (tmux list-sessions)
    if test -z "$ID"
        tmux new-session
        return
    end

    set new_session "Create New Session" 
    set ID (echo $ID\n$new_session | peco --on-cancel=error | cut -d: -f1)
    if test "$ID" = "$new_session"
        tmux new-session
    else if test -n "$ID"
        tmux attach-session -t "$ID"
    end
end

if test -z $TMUX && status --is-login
    attach_tmux_session_if_needed
end

#view
set -g theme_display_date yes
set -g theme_date_format "+%F %H:%M"
set -g theme_display_git_default_branch yes
set -g theme_color_scheme dark
set fish_plugins theme peco

# Setting PATH for Python 3.12
# The original version is saved in /Users/kei/.config/fish/config.fish.pysave
set -x PATH "/Library/Frameworks/Python.framework/Versions/3.12/bin" "$PATH"
