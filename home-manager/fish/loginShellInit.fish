if test -f $HOME/.local/mocword-data/mocword.sqlite
else
	mkdir -p $HOME/.local/mocword-data/
	curl -L -o $HOME/.local/mocword-data/mocword.sqlite.gz https://github.com/high-moctane/mocword-data/releases/download/eng20200217/mocword.sqlite.gz
	gzip -d $HOME/.local/mocword-data/mocword.sqlite.gz 
end

if not pgrep -x yaskkserv2 > /dev/null
	if not test -f $HOME/.skk-dict/dict.yaskkserv2
		yaskkserv2_make_dictionary --dictionary-filename=$HOME/.skk-dict/dict.yaskkserv2 $HOME/.skk-dict/SKK-JISYO.L 
	end
	yaskkserv2 ~/.skk-dict/dict.yaskkserv2 &
end
