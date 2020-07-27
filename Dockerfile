FROM ubuntu:latest
MAINTAINER tester

ENV USER tester
ENV HOME /home/${USER}
ENV SHELL /bin/bash

RUN useradd -m ${USER}
RUN gpasswd -a ${USER} sudo
RUN echo "${USER}:test_pass" | pass
RUN sed -i.bak "s#${HOME}:#${HOME}:${SHELL}#" /etc/passwd

RUN apt install git
RUN apt install vim

RUN git clone https://github.com/keimoriyama/dotfiles.git

WORKDIR dotfiles

