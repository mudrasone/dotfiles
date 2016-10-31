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

export HTML_TIDY=~/.tidyrc
export TERM='xterm-256color'
export PROJECT_HOME=$PWD/.virtualenv
export WORKON_HOME=$PWD/.virtualenv
export NVIMRC=~/.config/nvim/init.vim
export NVM_DIR="/Users/brandon/.nvm"

source /usr/local/bin/virtualenvwrapper.sh

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

function rmdss () {
    find . -name '*.DS_Store' -type f -delete
}

[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
if [ -e /Users/brandon/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/brandon/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
