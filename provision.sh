#!/bin/bash
U=vagrant
apt-get update -y
apt-get install -y apt-transport-https ca-certificates curl
apt-key adv --keyserver hkp://ha.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
echo "deb https://apt.dockerproject.org/repo ubuntu-trusty main" | tee /etc/apt/sources.list.d/docker.list
apt-get update -y
apt-cache policy docker-engine
apt-get install -y docker-engine
service docker start
gpasswd -a $U docker
sudo curl -o /usr/local/bin/docker-compose -L "https://github.com/docker/compose/releases/download/1.9.0/docker-compose-$(uname -s)-$(uname -m)"
chmod +x /usr/local/bin/docker-compose
chown -R $U /usr/local/bin/docker-compose
mkdir -p /opt/code
chown -R $U /opt/code
bash -c "curl https://civiclabsconsulting.github.io/dotfiles/install | sh"
exit 0
