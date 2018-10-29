#!/bin/bash

sudo apt-get update -y
sudo apt-get install -y tmux vim-gtk zsh silversearcher-ag git python3-pip docker.io

pip3 install docker-compose

mkdir -p ~/code ~/.local/bin

# Zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# Z
wget https://github.com/rupa/z/raw/master/z.sh ~/.local/bin/z.sh

# FZF
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install

# Vim
mkdir -p ~/.vim/autoload ~/.vim/bundle && \
    curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

git clone git@github.com:altercation/vim-colors-solarized.git ~/.vim/bundle/vim-colors-solarized
git clone git@github.com:junegunn/fzf.vim.git ~/.vim/bundle/fzf.vim
git clone git@github.com:scrooloose/nerdtree.git ~/.vim/bundle/nerdtree
git clone git@github.com:gioele/vim-autoswap.git ~/.vim/bundle/vim-autoswap
git clone git@github.com:vim-syntastic/syntastic.git ~/.vim/bundle/syntastic

# Setup
git clone git@github.com:pindaroso/dotfiles.git ~/code/dotfiles

cd code/dotfiles

cp .vimrc .tmux.conf .zshrc ~
cp crcandy.zsh-theme ~/.oh-my-zsh/themes/crcandy.zsh-theme
