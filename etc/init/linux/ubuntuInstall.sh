#!/bin/sh

apt -y update

apt -y upgrade

apt install git

apt install build-essential libbz2-dev libdb-dev \
  libreadline-dev libffi-dev libgdbm-dev liblzma-dev \
  libncursesw5-dev libsqlite3-dev libssl-dev \
  zlib1g-dev uuid-dev tk-dev

apt install -y python3-pip

# install python liblary
pip3 install pynvim

#install zsh
apt install -y zsh
apt install -y nodejs npm
#install vim
apt install -y vim nvim

# install tmux
apt install -y tmux

#install tig
apt install -y tig

sudo chsh -s $(which zsh)
