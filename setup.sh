#!/bin/bash

# Debian
sudo apt-get update -y
sudo apt-get install -y tmux vim-gtk zsh silversearcher-ag git \
    python3-pip docker.io exuberant-ctags

pip3 install docker-compose

mkdir -p $HOME/code $HOME/.local/bin

# Zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# Z
wget https://github.com/rupa/z/raw/master/z.sh $HOME/.local/bin/z.sh

# FZF
git clone --depth 1 https://github.com/junegunn/fzf.git $HOME/.fzf
$HOME/.fzf/install

# Vim
mkdir -p $HOME/.vim/autoload $HOME/.vim/bundle && \
    curl -LSso $HOME/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

# Plugins
cd $HOME/.vim/bundle

git clone git@github.com:altercation/vim-colors-solarized.git
git clone git@github.com:junegunn/fzf.vim.git
git clone git@github.com:scrooloose/nerdtree.git
git clone git@github.com:gioele/vim-autoswap.git
git clone git@github.com:vim-syntastic/syntastic.git
git clone git@github.com:Valloric/YouCompleteMe.git
git clone git@github.com:majutsushi/tagbar.git
git clone git@github.com:brooth/far.vim.git
git clone git@github.com:romainl/vim-cool.git

# YCM
cd $HOME/.vim/bundle/YouCompleteMe
git submodule update --init --recursive
python3 install.py --clang-completer --go-completer

# Setup
git clone git@github.com:pindaroso/dotfiles.git $HOME/code/dotfiles

cd code/dotfiles

cp .vimrc .tmux.conf .zshrc $HOME
