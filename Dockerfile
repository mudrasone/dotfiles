FROM ubuntu:17.04

# Docker
RUN apt-get update -y && apt-get install -y curl docker docker-compose

# Neovim
RUN apt-get install -y software-properties-common && \
    add-apt-repository -y ppa:neovim-ppa/unstable && \
    apt-get -y update

# Ruby, Python and Tmux
RUN apt-get install -y python-software-properties python-dev python-pip \
    python3-dev python3-pip mosh exuberant-ctags build-essential ruby \
    neovim ruby-dev tmux

# Neovim dependencies
RUN pip2 install --upgrade neovim && pip3 install --upgrade neovim && \
    gem install neovim

# User
ENV ME robot
RUN adduser $ME
RUN gpasswd -a $ME docker

# Neovim plugin manager
RUN mkdir -p /home/$ME/.local/share/nvim/site/autoload/ && \
    chown $ME -R /home/$ME
RUN curl -fLo /home/$ME/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Neovim config
RUN curl https://civiclabsconsulting.github.io/dotfiles/init.vim -o init.vim && \
    mkdir -p /home/$ME/.config/nvim && mv init.vim /home/$ME/.config/nvim
RUN nvim +PlugInstall +UpdateRemotePlugins +qall

# GitHub
RUN git config --global user.name "Brandon Stiles" && \
    git config --global user.email stilesbr1@gmail.com && \
    git config --global core.editor nvim

# Tmux
ADD ./docs/.tmux.conf /home/$ME/.tmux.conf

# Terminal
CMD /bin/bash
