FILES=(.zshrc .zshenv .config .tmux.conf .tigrc .vimrc .hammerspoon .p10k.zsh)
# 移動できたらリンクを実行する
for f in ${FILES[@]}; do
	ln -snfv $HOME/.dotfiles/$f $HOME/$f
	echo $f
done

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
(echo; echo 'eval "$(/opt/homebrew/bin/brew shellenv)"') >> /Users/kei/.zprofile
eval "$(/opt/homebrew/bin/brew shellenv)"
brew bundle

ln -snfv $HOME/.dotfiles/.config/aquaskk/keymap.conf $HOME/Library/Application\ Support/AquaSKK/keymap.conf

