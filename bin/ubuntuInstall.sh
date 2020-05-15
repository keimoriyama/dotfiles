#!/bin/sh

$ sudo apt-get update

sudo apt install git

sudo apt install build-essential libbz2-dev libdb-dev \
>   libreadline-dev libffi-dev libgdbm-dev liblzma-dev \
>   libncursesw5-dev libsqlite3-dev libssl-dev \
>   zlib1g-dev uuid-dev tk-dev

sudo apt install python3-pip


# install python liblary

pip3 install pynvim

#install nerd font
cd $HOME
git clone --branch=master --depth 1 https://github.com/ryanoasis/nerd-fonts.git
cd nerd-fonts
install.sh

#install zsh
sudo apt-get install zsh
chsh -s $(which zsh)

# install tmux
sudo apt-get install tmux

#install tig
sudo apt-get install tig

