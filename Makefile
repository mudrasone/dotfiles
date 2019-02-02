all:
	@cp ~/.tmux.conf ~/.zshrc ~/.vimrc ~/.config/kitty/kitty.conf .

osx:
	@brew list > .brew
	@brew cask list > .casks
