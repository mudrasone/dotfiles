#!/bin/bash
U=vagrant
sudo apt-get update -y
sudo apt-get install -y apt-transport-https ca-certificates curl
sudo apt-key adv --keyserver hkp://ha.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
sudo echo "deb https://apt.dockerproject.org/repo ubuntu-trusty main" | tee /etc/apt/sources.list.d/docker.list
sudo apt-get update -y
sudo apt-cache policy docker-engine
sudo apt-get install -y docker-engine
sudo service docker start
sudo gpasswd -a $U docker
sudo sudo curl -o /usr/local/bin/docker-compose -L "https://github.com/docker/compose/releases/download/1.9.0/docker-compose-$(uname -s)-$(uname -m)"
sudo chmod +x /usr/local/bin/docker-compose
sudo chown -R $U /usr/local/bin/docker-compose
sudo mkdir -p /opt/code
sudo chown -R $U /opt/code
sudo curl https://civiclabsconsulting.github.io/dotfiles/install | sh
exit 0
