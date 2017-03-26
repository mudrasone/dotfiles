all:
	@cp ~/.tmux.conf ~/.emacs ~/.config/nvim/init.vim ~/.bashrc ~/.zshrc .

serve:
	@cd _site && jekyll serve

