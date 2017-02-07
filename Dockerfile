FROM ubuntu:17.04
RUN apt-get update -y && apt-get install -y curl docker docker-compose
ENV U robot
RUN adduser $U
RUN service docker start
RUN gpasswd -a $U docker
RUN mkdir -p /opt/code && chown -R $U /opt/code
RUN curl https://civiclabsconsulting.github.io/dotfiles/install | sh
