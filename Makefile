all:
	@rm -rf .emacs.d | echo "Skipping..."
	@mkdir .emacs.d
	@mkdir .config
	@cp -R ~/.tmux.conf ~/.config ~/.functionsrc ~/.zshrc ~/.vimrc .
	@cp -R ~/.emacs.d/personal ~/.emacs.d/prelude-modules.el .emacs.d/

serve:
	@cd _site && jekyll serve
