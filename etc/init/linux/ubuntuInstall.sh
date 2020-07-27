#!/bin/sh

sudo apt -y update

sudo apt -y upgrade

sudo apt install git

sudo apt install build-essential libbz2-dev libdb-dev \
  libreadline-dev libffi-dev libgdbm-dev liblzma-dev \
  libncursesw5-dev libsqlite3-dev libssl-dev \
  zlib1g-dev uuid-dev tk-dev

sudo apt install -y python3-pip

# install python liblary
sudo pip3 install pynvim

#install zsh
sudo apt install -y zsh
sudo apt install -y nodejs npm
#install vim
sudo apt install -y vim nvim

# install tmux
sudo apt install -y tmux

#install tig
sudo apt install -y tig

sudo chsh -s $(which zsh)
