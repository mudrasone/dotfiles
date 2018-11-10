all:
	@cp ~/.tmux.conf ~/.zshrc ~/.functionsrc ~/.vimrc .


linux:
	@echo "?"

osx:
	@brew list > .brew
	@brew cask list > .casks
