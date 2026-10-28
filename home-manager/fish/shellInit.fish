fish_add_path --global --move --path "$HOME/.nix-profile/bin" "/etc/profiles/per-user/$USER/bin" "/nix/var/nix/profiles/default/bin"
fish_add_path --global --append --path "/opt/homebrew/bin" "$HOME/.cargo/bin" "$HOME/.local/bin" "$HOME/.roswell/bin"
set -gx SHELL "/etc/profiles/per-user/$USER/bin/fish"

# Fish git prompt 
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red

# Status Chars
set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_untrackedfiles '☡'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '+'
set __fish_git_prompt_char_upstream_behind '-'
