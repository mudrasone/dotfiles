#!/bin/bash

set -e
set -x

# Docker Firewall
# https://unix.stackexchange.com/questions/331640/cant-add-docker0-interface-to-trusted-zone-with-firewalld
firewall-cmd --permanent --zone=trusted --add-interface=docker0
firewall-cmd --direct --permanent --add-rule ipv4 filter INPUT 3 -i docker0 -j ACCEPT
firewall-cmd --reload

# Mosh Firewall
# sudo iptables -I INPUT 1 -p udp --dport 60000:61000 -j ACCEPT

systemctl restart docker
