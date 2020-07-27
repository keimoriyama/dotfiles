FROM ubuntu:latest

RUN apt-get update && apt-get -y upgrade
RUN apt-get install -y git \
                      vim

RUN git clone https://github.com/keimoriyama/dotfiles.git

WORKDIR /dotfiles


