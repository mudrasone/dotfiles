export HTML_TIDY=~/.tidyrc
export TERM='screen-256color'
export PROJECT_HOME=$PWD/.virtualenv
export WORKON_HOME=$PWD/.virtualenv
export NVIMRC=~/.config/nvim/init.vim
export NVM_DIR="/Users/brandon/.nvm"
export MANPATH=$NIX_LINK/share/man:$MANPATH

export PATH=/bin:/usr/local/bin:/usr/bin:/usr/sbin:/sbin/usr/local/sbin:$PATH
export PATH=/Library/Frameworks/UnixImageIO.framework/Programs:$PATH
export PATH=/Library/Frameworks/PROJ.framework/Programs:$PATH
export PATH=/Library/Frameworks/SQLite3.framework/Programs:$PATH
export PATH=/Library/Frameworks/GEOS.framework/Programs:$PATH
export PATH=/Library/Frameworks/GDAL.framework/Programs:$PATH
export PATH=/usr/local/pgsql/bin:$PATH
export PATH=$HOME/Library/Haskell/bin:$PATH
export PATH=$HOME/.cabal:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$NIX_LINK/bin:$NIX_LINK/sbin:$PATH
export PATH=/Library/TeX/texbin:$PATH

# OPAM configuration
. /Users/brandon/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

alias emacs=/usr/local/bin/emacs
alias dcp='docker-compose'
alias dm='/usr/local/bin/docker-machine'
alias vim='nvim'
alias vi='nvim'
alias ctags="`brew --prefix`/bin/ctags"
alias bfg="git filter-branch --tree-filter 'rm -rf $@' HEAD"
alias tmux="TERM=screen-256color tmux -2"
alias ls="/bin/ls -G"

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

function cleandocker {
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

function cleandockerf {
    docker rm $(docker ps -a -q)
    docker rmi $(docker images -q)
    docker volume rm $(docker volume ls |awk '{print $2}')
    rm -rf ~/Library/Containers/com.docker.docker/Data/*
}

source /usr/local/bin/virtualenvwrapper.sh
source $HOME/.profile

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
if [ -e /Users/brandon/.nix-profile/etc/profile.d/nix.sh ]; then
    . /Users/brandon/.nix-profile/etc/profile.d/nix.sh;
fi # added by Nix installer

# This is one of many color schemes for fzf. Check the fzf wiki for more
gen_fzf_default_opts() {
  local base03="234"
  local base02="235"
  local base01="240"
  local base00="241"
  local base0="244"
  local base1="245"
  local base2="254"
  local base3="230"
  local yellow="136"
  local orange="166"
  local red="160"
  local magenta="125"
  local violet="61"
  local blue="33"
  local cyan="37"
  local green="64"

  # fzf uses ncurses for it's UI. ncurses doesn't support 24-bit color, and
  # last time I tried, I couldn't get the ANSI 16 colors to play nicely.

  # Solarized Dark color scheme for fzf
  export FZF_DEFAULT_OPTS_DARK="
    --color fg:-1,bg:-1,hl:$blue,fg+:$base2,bg+:-1,hl+:$blue
    --color info:$yellow,prompt:$yellow,pointer:$base3,marker:$base3,spinner:$yellow
  "
  # Solarized Light color scheme for fzf
  export FZF_DEFAULT_OPTS_LIGHT="
    --color fg:-1,bg:-1,hl:$blue,fg+:$base02,bg+:-1,hl+:$blue
    --color info:$yellow,prompt:$yellow,pointer:$base03,marker:$base03,spinner:$yellow
  "

  if [ "$SOLARIZED" = "dark" ]; then
    export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS_DARK"
  elif [ "$SOLARIZED" = "light" ]; then
    export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS_LIGHT"
  else
    export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS_LIGHT"
  fi
}
