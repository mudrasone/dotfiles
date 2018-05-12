all:
	@rm -rf .emacs.d && mkdir .emacs.d | echo "Skipping..."
	@cp ~/.tmux.conf ~/.zshrc ~/.functionsrc ~/.vimrc .
	@cp -R ~/.emacs.d/personal ~/.emacs.d/prelude-modules.el .emacs.d/
	@rm .emacs.d/personal/custom.el
	@brew list > .brew
	@brew cask list > .casks
