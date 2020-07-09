all:
	@cp ~/.tmux.conf ~/.zshrc ~/.vimrc .

osx:
	@brew list > .brew
	@brew cask list > .casks
