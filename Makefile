all:
	@cp ~/.tmux.conf ~/.zshrc ~/.vimrc .


linux:
	@echo "?"

osx:
	@brew list > .brew
	@brew cask list > .casks
