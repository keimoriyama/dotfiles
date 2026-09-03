set -x PATH "/nix/var/nix/profiles/default/bin" "$PATH"
set -x PATH "/etc/profiles/per-user/kei/bin/" "$PATH"
set -x PATH "$HOME/.nix-profile/bin" "$PATH"
set -x PATH "$PATH" "/opt/homebrew/bin"
set -x PATH "$PATH" "$HOME/.cargo/bin" 
set -x PATH "$PATH" "$HOME/.local/bin"
set -x PATH "$PATH" "$HOME/.roswell/bin/"
set -gx SHELL "/etc/profiles/per-user/kei/bin/fish"

# cage サンドボックス下で claude を起動する際に注入する注意文 (cala abbr で使用)。
set -gx __CAGE_SANDBOX_NOTE "You are running under the cage sandbox: file writes are allowed only in the project directory, caches, and temp dirs. A write denial outside those is expected — do not retry with sudo/chmod or try to work around the sandbox; work within the writable paths."

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
