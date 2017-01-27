#!/bin/bash

add-apt-repository -y ppa:neovim-ppa/unstable
apt-get update -y && apt-get install -y software-properties-common  \
    python-software-properties neovim python-dev python-pip \
    python3-dev python3-pip
