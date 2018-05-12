all:
	@rm -rf .emacs.d | echo "Skipping..."
	@mkdir .emacs.d
	@cp ~/.tmux.conf ~/.zshrc ~/.vimrc .
	@cp -R ~/.emacs.d/personal ~/.emacs.d/prelude-modules.el .emacs.d/

