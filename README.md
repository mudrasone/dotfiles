# Dotfiles

![Screenshot](https://raw.githubusercontent.com/civiclabsconsulting/dotfiles/master/screenshot.png)

## Keyboard

* Switch control and caps lock keys for easier keyboard use

## Terminal

iTerm is great, a bit more robust than OSX's default terminal, Terminal.app.

## Tmux

### Notes

To get `solarized` colors working properly it's necessary to install a modified version of `tmux` with 24-bit color support.

```
brew tap choppsv1/term24
brew install choppsv1/term24/tmux
```

* See .tmux.conf for full configuration details
* Prefix is ^a

### Basic Commands

* New session: tmux new -s <name>
* Rename session: prefix + $
* Split screen horizontally: prefix + "
* Split screen vertically: prefix + %
* Next session: prefix + )
* Previous session: prefix + (
* Navigate windows:  prefix + hjkl
* New window aka tab: prefix + c
* Close window: exit
* Rename session: tmux rename-session -t <old name> <new name>
* Attach session: tmux attach-session -t <name>
* Swap panes: prefix + ^o
* Toggle panes: prefix + spacebar
* Increase pane size: prefix + :resize-pane -(U | D | L | R) 10

## Neovim

Currently using Neovim which can be installed via pip3: `pip3 install neovim`. A great tutorial for learning Vim can be found [here](http://yannesposito.com/Scratch/en/blog/Learn-Vim-Progressively/).

This setup requires ctags, which can be installed with `brew install ctags`.

### Basic Commands

* Move cursor down: j
* Move cursor up: k
* Move cursor left: h
* Move cursor right: l
* Cut: x
* Cut line: dd
* Copy visual selection: y
* Copy line: yy
* Substitue: s
* Insert line below: o
* Paste: p
* Paste above line: P
* NERDTree mode: ^e
* Full screen forward: ^f
* Full screen back: ^b
* 1/2 screen: ^u
* 1/2 screen: ^d
* Auto-format entire file: gg=G
* Auto-format line: ==
* Vertical split: vsplit
* Horizontal split: split
* Execute command: !<command>
