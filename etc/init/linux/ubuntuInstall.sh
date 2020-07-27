#!/bin/sh

apt-get -y update

apt-get -y  upgrade

apt install git

apt install -y build-essential libbz2-dev libdb-dev \
>   libreadline-dev libffi-dev libgdbm-dev liblzma-dev \
>   libncursesw5-dev libsqlite3-dev libssl-dev \
>   zlib1g-dev uuid-dev tk-dev

apt install -y python3-pip


# install python liblary

pip3 install pynvim

#install zsh
apt-get -y install zsh
chsh -s $(which zsh)

#install vim
apt-get -y install vim nvim

# install tmux
apt-get -y install tmux

#install tig
apt-get -y install tig

