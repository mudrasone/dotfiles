export PROJECT_HOME=$PWD/.virtualenv
export WORKON_HOME=$PROJECT_HOME
export NVIMRC=$HOME/.config/nvim/init.vim
export NVM_DIR=$HOME/.nvm
export NIX_LINK=$HOME/.nix-profile
export MANPATH=$NIX_LINK/share/man:$MANPATH
export GPG_TTY=`tty`
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'

export PATH=/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/sbin:$PATH
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
export PATH=$HOME/.rvm/bin:$PATH
export PATH=$NIX_LINK/bin:$NIX_LINK/sbin:$PATH

. $HOME/.opam/opam-init/init.zsh
. /usr/local/bin/virtualenvwrapper.sh
. /usr/local/etc/profile.d/z.sh
. /usr/local/opt/nvm/nvm.sh
. $HOME/.functionsrc

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

if [ -e /Users/brandon/.nix-profile/etc/profile.d/nix.sh ]; then
    source /Users/brandon/.nix-profile/etc/profile.d/nix.sh;
fi

[ -f ~/.gpg-agent-info ] && source ~/.gpg-agent-info
if [ -S "${GPG_AGENT_INFO%%:*}" ]; then
    export GPG_AGENT_INFO
else
    eval $(gpg-agent --daemon --write-env-file ~/.gpg-agent-info)
fi

if [ -f `brew --prefix`/etc/bash_completion ]; then
    . `brew --prefix`/etc/bash_completion
fi

if [[ -n "$EMACS" ]]; then
    export TERM='eterm-color'
    PS1="\[\033[01;32m\]\u@\h\[\033[01;34m\] \W\[\033[00m\]\n$ "
else 
    export TERM='xterm-256color'
fi

# added by travis gem
[ -f /Users/brandon/.travis/travis.sh ] && source /Users/brandon/.travis/travis.sh
