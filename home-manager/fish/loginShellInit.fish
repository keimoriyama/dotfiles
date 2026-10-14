if test -f $HOME/.local/mocword-data/mocword.sqlite
else
	mkdir -p $HOME/.local/mocword-data/
	curl -L -o $HOME/.local/mocword-data/mocword.sqlite.gz https://github.com/high-moctane/mocword-data/releases/download/eng20200217/mocword.sqlite.gz
	gzip -d $HOME/.local/mocword-data/mocword.sqlite.gz 
end
