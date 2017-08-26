#!/bin/bash

source $HOME/.zshenv
source $HOME/.functionsrc
source $HOME/.opam/opam-init/init.zsh

source $(brew --prefix autoenv)/activate.sh

source /usr/local/bin/virtualenvwrapper.sh
source /usr/local/etc/profile.d/z.sh
source /usr/local/opt/nvm/nvm.sh

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

# Added by travis gem
[ -f /Users/brandon/.travis/travis.sh ] && source /Users/brandon/.travis/travis.sh
