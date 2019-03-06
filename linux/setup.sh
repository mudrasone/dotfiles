#!/bin/bash

set -x
# set -e

# Debian"
sudo apt-get update -y
sudo dpkg --configure -a
sudo apt-get install -y tmux vim-gtk zsh silversearcher-ag git \
    python3-pip exuberant-ctags curl cmake npm htop \
    gnome-tweak-tool mosh

# Docker CE
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable edge"
apt-cache policy docker-ce
sudo apt-get install -y docker-ce

# Nvidia Docker
curl -s -L https://nvidia.github.io/nvidia-docker/gpgkey | \
    sudo apt-key add -
distribution=$(. /etc/os-release;echo $ID$VERSION_ID)
curl -s -L https://nvidia.github.io/nvidia-docker/$distribution/nvidia-docker.list | \
    sudo tee /etc/apt/sources.list.d/nvidia-docker.list
sudo apt-get update
sudo apt-get install -y nvidia-docker2
sudo pkill -SIGHUP dockerd
docker run --runtime=nvidia --rm nvidia/cuda:9.0-base nvidia-smi # Test

# GitHub
git config --global user.email "me@pindaroso.com"
git config --global user.username "pindaroso"
git config --global user.name "Pindaroso"

sudo usermod -aG docker $USER
pip3 install docker-compose

mkdir -p $HOME/code $HOME/.local/bin

# Zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# Z
wget https://github.com/rupa/z/raw/master/z.sh
mv z.sh $HOME/.local/bin/z.sh
chmod +x $HOME/.local/bin/z.sh

# FZF
git clone --depth 1 https://github.com/junegunn/fzf.git $HOME/.fzf
$HOME/.fzf/install

# Vim
mkdir -p $HOME/.vim/autoload $HOME/.vim/bundle && \
    curl -LSso $HOME/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

# GO
wget https://dl.google.com/go/go1.11.2.linux-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.11.2.linux-amd64.tar.gz
rm go1.11.2.linux-amd64.tar.gz

# Elm
sudo npm install -g elm

# Haskell Stack
curl -sSL https://get.haskellstack.org/ | sh

# Plugins
cd $HOME/.vim/bundle

git clone git@github.com:altercation/vim-colors-solarized.git
git clone git@github.com:junegunn/fzf.vim.git
git clone git@github.com:scrooloose/nerdtree.git
git clone git@github.com:gioele/vim-autoswap.git
git clone git@github.com:vim-syntastic/syntastic.git
git clone git@github.com:Valloric/YouCompleteMe.git
git clone git@github.com:majutsushi/tagbar.git
git clone git@github.com:dkprice/vim-easygrep.git
git clone git@github.com:romainl/vim-cool.git
git clone git@github.com:ElmCast/elm-vim.git
git clone git@github.com:tomlion/vim-solidity.git
git clone git@github.com:chr4/nginx.vim.git
git clone git@github.com:ludovicchabant/vim-gutentags.git
git clone git@github.com:Chiel92/vim-autoformat.git
git clone git@github.com:airblade/vim-gitgutter.git

# YCM
cd $HOME/.vim/bundle/YouCompleteMe
git submodule update --init --recursive
python3 install.py --clang-completer --go-completer

# Gnome Term
git clone git@github.com:Anthony25/gnome-terminal-colors-solarized.git
cd gnome-terminal-colors-solarized
./install.sh

# Dotfiles
cd ~/code/dotfiles
cp .vimrc .tmux.conf .zshrc $HOME
mkdir -p $HOME/.vim/{.backup,.swap,.undo}

# Dapp Tools
curl https://dapp.tools/install | sh

# Kitty
curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin
cp kitty.conf ~/.config/kitty/kitty.conf

# Nix
curl https://nixos.org/nix/install | sh

# Font
# https://github.com/tonsky/FiraCode

# Travis
sudo apt-add-repository -y ppa:brightbox/ruby-ng
sudo apt-get update
sudo apt-get install -y ruby-dev ruby-switch
gem install travis -v 1.8.9 --no-rdoc --no-ri
