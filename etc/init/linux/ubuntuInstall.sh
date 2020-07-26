#!/bin/sh

apt-get update

apt-get upgrade

apt install git

apt install build-essential libbz2-dev libdb-dev \
>   libreadline-dev libffi-dev libgdbm-dev liblzma-dev \
>   libncursesw5-dev libsqlite3-dev libssl-dev \
>   zlib1g-dev uuid-dev tk-dev

apt install python3-pip


# install python liblary

pip3 install pynvim

#install zsh
apt-get install zsh
chsh -s $(which zsh)

#install vim
apt-get install vim

# install tmux
apt-get install tmux

#install tig
apt-get install tig
