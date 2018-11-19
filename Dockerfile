FROM ubuntu:18.04
RUN apt-get update && apt-get -y install sudo
RUN adduser --disabled-password --gecos '' docker
RUN adduser docker sudo
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
USER docker
WORKDIR /home/docker
ADD ./linux/setup.sh /home/docker/setup.sh
ADD .zshrc .tmux.conf .vimrc /home/docker/
RUN /home/docker/setup.sh
