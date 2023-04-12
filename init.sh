#!/bin/bash

# 手動でリンクを貼る
ln -sf ~/.dotfiles/.config/ $HOME
# 移動できたらリンクを実行する
for f in .??*
do
    [ "$f" = ".git" ] && continue
    [ "$f" = ".config" ] && continue
	rm -rf "~/dotfiles/$f"
    ln -sf "~/.dotfiles/$f" "$HOME/$f"
done
