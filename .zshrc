export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="avit"

plugins=(git fasd history-substring-search)

source $ZSH/oh-my-zsh.sh
source $HOME/.bashrc

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

if [[ $USER == "root" ]]; then
  CARETCOLOR="red"
else
  CARETCOLOR="brown"
fi

export ZSH_THEME_GIT_TIME_SINCE_COMMIT_NEUTRAL="%{$fg[brown]%}"
export ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[brown]%}◒ "
