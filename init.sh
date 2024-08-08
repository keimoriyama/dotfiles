FILES=(.zshrc .zshenv .config .tmux.conf .tigrc .vimrc .hammerspoon .p10k.zsh)
# 移動できたらリンクを実行する
for f in ${FILES[@]}; do
	ln -snfv $HOME/.dotfiles/$f $HOME/$f
	echo $f
done

if [["$OSTYPE" == 'darwin'*]]; then 
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
	(echo; echo 'eval "$(/opt/homebrew/bin/brew shellenv)"') >> /Users/kei/.zprofile
	eval "$(/opt/homebrew/bin/brew shellenv)"
	brew bundle

	ln -snfv $HOME/.dotfiles/.config/aquaskk/keymap.conf $HOME/Library/Application\ Support/AquaSKK/keymap.conf


	curl https://git.io/fisher --create-dirs -sLo ~/.config/fish/functions/fisher.fish
	fisher install jethrokuan/z
	fisher install oh-my-fish/plugin-peco
	fisher install IlanCosman/tide@v6

	cargo install mocword
fi
