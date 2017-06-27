export TERM='xterm-256color'
export PROJECT_HOME=$PWD/.virtualenv
export WORKON_HOME=$PROJEECT_HOME
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
export PATH=/usr/local/sbin:$PATH
export PATH=$NIX_LINK/bin:$NIX_LINK/sbin:$PATH
export PATH=$HOME/.rvm/bin:$PATH

. /Users/brandon/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
. /usr/local/bin/virtualenvwrapper.sh
. /usr/local/etc/profile.d/z.sh
. /usr/local/opt/nvm/nvm.sh

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

if [ -e /Users/brandon/.nix-profile/etc/profile.d/nix.sh ]; then
    source /Users/brandon/.nix-profile/etc/profile.d/nix.sh;
fi

[ -f ~/.gpg-agent-info ] && source ~/.gpg-agent-info
if [ -S "${GPG_AGENT_INFO%%:*}" ]; then
    export GPG_AGENT_INFO
else
    eval $( gpg-agent --daemon --write-env-file ~/.gpg-agent-info )
fi

if [ -f `brew --prefix`/etc/bash_completion ]; then
    . `brew --prefix`/etc/bash_completion
fi

function git-lazy () {
    git add .
    git commit -a -m "$1"
    git push
}

function git-filter-rm () {
    git filter-branch --tree-filter 'rm -rf $@' HEAD
}

function find-rm () {
    find . -name "$1" -type f -delete
}

function dns-flush () {
    sudo dscacheutil -flushcache
    sudo killall -HUP mDNSResponder
}

function docker-clean-volumes () {
    docker run -v /var/run/docker.sock:/var/run/docker.sock -v /var/lib/docker:/var/lib/docker --rm martin/docker-cleanup-volumes --dry-run
    docker run --rm -v /var/run/docker.sock:/var/run/docker.sock -v /etc:/etc spotify/docker-gc
}

function docker-machine-create () {
    if [ $# -eq 0 ]
    then
	echo "Usage: docker-machine-create <name>"
    else
	docker-machine create --driver virtualbox --virtualbox-memory 8000 --virtualbox-disk-size 40000 "$1"
    fi
}

function docker-machine-env () {
    if [ $# -eq 0 ]
    then
	echo "Usage: docker-machine-env <name>"
    else
	eval $(docker-machine env "$1")
    fi
}

alias c="docker-compose"
alias m="/usr/local/bin/docker-machine"
alias d="/usr/local/bin/docker"
alias ctags="`brew --prefix`/bin/ctags"
alias l="/bin/ls -aG"

if [[ -n "$EMACS" ]]; then
    export TERM='eterm-color'
    PS1="\[\033[01;32m\]\u@\h\[\033[01;34m\] \W\[\033[00m\]\n$ "
fi
