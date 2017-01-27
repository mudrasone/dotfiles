update:
	@cp ~/.tmux.conf ~/.config/nvim/init.vim ~/.bash_profile ~/.bashrc .

all:
	@rm -rf docs
	@mkdir docs
	@cp install init.vim colortest.pl .tmux.conf .bashrc .bash_profile ./docs
