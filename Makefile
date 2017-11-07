all:
	@cp ~/.tmux.conf ~/.functionsrc ~/.zshrc .

serve:
	@cd _site && jekyll serve
