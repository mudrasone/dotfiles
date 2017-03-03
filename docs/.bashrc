export HTML_TIDY=~/.tidyrc
export TERM='xterm-256color'
export PROJECT_HOME=$PWD/.virtualenv
export WORKON_HOME=$PWD/.virtualenv
export NVIMRC=~/.config/nvim/init.vim
export NVM_DIR="/Users/brandon/.nvm"
export MANPATH=$NIX_LINK/share/man:$MANPATH
export GPG_TTY=`tty`
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'

export PATH=/bin:/usr/local/bin:/usr/bin:/usr/sbin:/sbin/usr/local/sbin:$PATH
export PATH=/Library/Frameworks/UnixImageIO.framework/Programs:$PATH
export PATH=/Library/Frameworks/PROJ.framework/Programs:$PATH
export PATH=/Library/Frameworks/SQLite3.framework/Programs:$PATH
export PATH=/Library/Frameworks/GEOS.framework/Programs:$PATH
export PATH=/Library/Frameworks/GDAL.framework/Programs:$PATH
export PATH=/Library/TeX/texbin:$PATH
export PATH=/usr/local/pgsql/bin:$PATH
export PATH=$HOME/Library/Haskell/bin:$PATH
export PATH=$HOME/.cabal:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$NIX_LINK/bin:$NIX_LINK/sbin:$PATH
export PATH=$HOME/.rvm/bin:$PATH

# OPAM configuration
. /Users/brandon/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

alias emacs=/usr/local/bin/emacs
alias dcp='docker-compose'
alias dm='/usr/local/bin/docker-machine'
alias vim='nvim'
alias vi='nvim'
alias ctags="`brew --prefix`/bin/ctags"
alias bfg="git filter-branch --tree-filter 'rm -rf $@' HEAD"
alias tmux="TERM=xterm-256color tmux -2"
alias ls="/bin/ls -G"

source /usr/local/bin/virtualenvwrapper.sh
source $HOME/.profile

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

if [ -e /Users/brandon/.nix-profile/etc/profile.d/nix.sh ]; then
    . /Users/brandon/.nix-profile/etc/profile.d/nix.sh;
fi

# GPG
[ -f ~/.gpg-agent-info ] && source ~/.gpg-agent-info
if [ -S "${GPG_AGENT_INFO%%:*}" ]; then
  export GPG_AGENT_INFO
else
  eval $( gpg-agent --daemon --write-env-file ~/.gpg-agent-info )
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

function lazygit () {
    git add .
    git commit -a -m "$1"
    git push
}

function rmdss {
    find . -name '*.DS_Store' -type f -delete
}

function rmpyc {
    find . -name '*.pyc' -type f -delete
}

function tinit {
    cd ~/Code/"$1"
    tmux -2 new-session -d -s "$1"
    tmux -2 split-window -h -p 40 htop
    tmux -2 split-window -v
    tmux -2 attach-session -d -t "$1"
}

function flushdns {
    sudo dscacheutil -flushcache;
    sudo killall -HUP mDNSResponder;
}

function docker-clean-tmp {
    screen ~/Library/Containers/com.docker.docker/Data/com.docker.driver.amd64-linux/tty

    # Next login as root

    dd if=/dev/zero of=/var/tempfile

    rm /var/tempfile

    # logout of the VM
    # Quit the Docker client entirely
    # Now we can recompress the disk:

    # > pwd
    # /Users/nick/Library/Containers/com.docker.docker/Data/com.docker.driver.amd64-linux

    mv Docker.qcow2 Docker.qcow2.original
    du -hs Docker.qcow2.original
    qemu-img convert -O qcow2 Docker.qcow2.original Docker.qcow2
    rm Docker.qcow2.original
    du -hs Docker.qcow2
}

function docker-clean-volumes {
    docker rm $(docker ps -a -q)
    docker rmi $(docker images -q)
    docker volume rm $(docker volume ls |awk '{print $2}')
    rm -rf ~/Library/Containers/com.docker.docker/Data/*
}
