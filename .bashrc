export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/.rvm/bin"

source /Users/brandon/.rvm/scripts/rvm
source ~/.bash_profile

# OPAM configuration
. /Users/brandon/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
