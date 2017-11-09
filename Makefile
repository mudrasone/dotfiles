all:
	@rm -rf .emacs.d | mkdir .emacs.d
	@cp -R ~/.tmux.conf ~/.functionsrc ~/.zshrc ~/.vimrc .
	@cp -R ~/.emacs.d/personal ~/.emacs.d/prelude-modules.el .emacs.d/

serve:
	@cd _site && jekyll serve
