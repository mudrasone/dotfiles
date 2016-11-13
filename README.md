# Dotfiles

![Screenshot](https://raw.githubusercontent.com/civiclabsconsulting/dotfiles/master/screenshot.png)

## Keyboard

* Switch `control` and `caps lock` keys for easier keyboard ergonomics with Vim.

## Terminal

iTerm is great, a bit more robust than OSX's default terminal, Terminal.app. iTerm 3.0 supports ligatures which can be useful.

## Tmux

### Notes

To get `tmux` working properly with 24-bit color support, the following patched libary is needed: `choppsv1/term24`.

```
brew tap choppsv1/term24
brew install choppsv1/term24/tmux
```

* Prefix is ^a
* See .tmux.conf for full configuration details

## Neovim

Currently using Neovim which can be installed via pip3: `pip3 install neovim`. A great tutorial for learning Vim can be found [here](http://yannesposito.com/Scratch/en/blog/Learn-Vim-Progressively/).

This setup requires ctags, which can be installed with `brew install ctags`.
