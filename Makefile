all:
	@cp ~/.tmux.conf ~/.functionsrc ~/.config/nvim/init.vim ~/.bashrc ~/.zshrc .
	@cp -R ~/.emacs.d/init.el .emacs.d/init.el
	@cp -R ~/.emacs.d/settings.org .emacs.d/settings.org
	@cp -R ~/.emacs.d/bookmarks .emacs.d/bookmarks

serve:
	@cd _site && jekyll serve

