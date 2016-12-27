export NIXLINK=$HOME/.nix-profile
export HTML_TIDY=~/.tidyrc
export TERM='screen-256color'
export PROJECT_HOME=$PWD/.virtualenv
export WORKON_HOME=$PWD/.virtualenv
export NVIMRC=~/.config/nvim/init.vim
export NVM_DIR="/Users/brandon/.nvm"
export MANPATH=$NIX_LINK/share/man:$MANPATH

export PATH=/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/sbin
export PATH=/Library/Frameworks/UnixImageIO.framework/Programs:$PATH
export PATH=/Library/Frameworks/PROJ.framework/Programs:$PATH
export PATH=/Library/Frameworks/SQLite3.framework/Programs:$PATH
export PATH=/Library/Frameworks/GEOS.framework/Programs:$PATH
export PATH=/Library/Frameworks/GDAL.framework/Programs:$PATH
export PATH=/usr/local/pgsql/bin:$PATH
export PATH=$HOME/Library/Haskell/bin:$PATH
export PATH=$HOME/.cabal:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.rvm/bin:$PATH
export PATH=$NIX_LINK/bin:$NIX_LINK/sbin:$PATH

# OPAM configuration
. /Users/brandon/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

source /usr/local/bin/virtualenvwrapper.sh
source /Users/brandon/.rvm/scripts/rvm

alias emacs=/usr/local/bin/emacs
alias dcp='docker-compose'
alias dm='/usr/local/bin/docker-machine'
alias tmux='tmux -2'
alias vim='nvim'
alias vi='nvim'
alias ctags="`brew --prefix`/bin/ctags"
alias bfg="git filter-branch --tree-filter 'rm -rf $@' HEAD"

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

function has-session {
    tmux has-session -t "$1" 2>/dev/null
}

function tinit {
    cd ~/Code/"$1"
    tmux -2 new-session -d -s "$1"
    tmux -2 split-window -h -p 40 htop
    tmux -2 split-window -v
    tmux -2 attach-session -d -t "$1"
}

[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
if [ -e /Users/brandon/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/brandon/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
