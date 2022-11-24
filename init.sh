#!/bin/bash
export PLATFORM

sudo xattr -dr com.apple.quarantine /Applications/Sourcetree.app
# install brew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# install via brew
brew bundle --file=./Brewfile
# 手動でリンクを貼る
ln -sf ~/.dotfiles/.config/ $HOME
# 移動できたらリンクを実行する
for f in .??*
do
    [ "$f" = ".git" ] && continue

    ln -snfv "$DOTPATH/$f" "$HOME"/"$f"
done
