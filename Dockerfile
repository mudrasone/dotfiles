FROM ubuntu:17.04
MAINTAINER https://www.civiclabs.com
ENV REFRESHED_AT 2017-02-07

# Docker
RUN apt-get update -y && apt-get install -y curl docker docker-compose

# Neovim
RUN apt-get install -y --no-install-recommends software-properties-common && \
    add-apt-repository -y ppa:neovim-ppa/unstable && \
    apt-get -y update

# Ruby, Python and Tmux
RUN apt-get install -y --no-install-recommends \
    python-software-properties python-dev python-pip neovim ruby-dev tmux \
    apt-transport-https git python-dev python3-dev python3-pip mosh \
    exuberant-ctags build-essential ruby zsh wget python-setuptools \
    python3-setuptools

# Neovim dependencies
RUN pip3 install neovim
RUN gem install neovim

# OS
RUN update-alternatives --install /usr/bin/vi vi /usr/bin/nvim 60 && \
    update-alternatives --install /usr/bin/vim vim /usr/bin/nvim 60 && \
    update-alternatives --install /usr/bin/editor editor /usr/bin/nvim 60

# Cleanup
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# User
ENV ME robot
RUN adduser $ME
RUN gpasswd -a $ME docker

# Neovim plugin manager
ENV HOME /work
WORKDIR ${HOME}
ENV CONFIG_DIR ${HOME}/.config/nvim
RUN mkdir -p ${CONFIG_DIR}/autoload/
ADD docs/init.vim ${CONFIG_DIR}/

# Neovim Plugin Manager
RUN curl -o ${CONFIG_DIR}/autoload/plug.vim https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Neovim config
RUN nvim -c "PlugInstall!"
RUN nvim -c "UpdateRemotePlugins!"

# GitHub
RUN git config --global user.name "Brandon Stiles" && \
    git config --global user.email stilesbr1@gmail.com && \
    git config --global core.editor nvim

# Tmux
ADD ./docs/.tmux.conf /home/$ME/.tmux.conf

# Zsh
ENV TERM dumb
RUN wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh || true
RUN ln -s /root/.oh-my-zsh /home/$ME/.oh-my-zsh && \
    ln -s  /root/.zshrc /home/$ME/.zshrc

# Command
CMD tail -f /dev/null
