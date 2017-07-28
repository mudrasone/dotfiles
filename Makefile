all:
	@cp ~/.tmux.conf ~/.functionsrc ~/.config/nvim/init.vim ~/.bashrc ~/.zshrc ~/.profile ~/.zshenv .
	@cp -R ~/.emacs.d .

serve:
	@cd _site && jekyll serve
