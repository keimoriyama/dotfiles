set -x PATH \"/nix/var/nix/profiles/default/bin\" \"$PATH\"
set -x PATH \"$HOME/.nix-profile/bin\" \"$PATH\"
set -x PATH \"$PATH\" \"/opt/homebrew/bin\"
set -x PATH \"$PATH\" \"$HOME/.cargo/bin\" 
set -x PATH \"$PATH\" \"$HOME/.local/bin\"
set -x PATH \"$PATH\" \"$HOME/.roswell/bin/\"
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

if test -f $HOME/.local/mocword-data/mocword.sqlite 
	echo 'mocword data is installed'
else
	mkdir $HOME/.local/mocword-data/
	curl -OL --output-dir $HOME/.local/mocword-data/ https://github.com/high-moctane/mocword-data/releases/download/eng20200217/mocword.sqlite.gz
	gzip -d $HOME/.local/mocword-data/mocword.sqlite.gz 
end

if pgrep -x yaskkserv2 > /dev/null
	echo 'yaskkserv2 is running'
else
	echo 'yaskkserv2 is not running'
	yaskkserv2_make_dictionary --dictionary-filename=$HOME/.skk-dict/dict.yaskkserv2 $HOME/.skk-dict/SKK-JISYO.L 
	yaskkserv2 ~/.skk-dict/dict.yaskkserv2 
end
