# dotfiles

![Screenshot](https://raw.githubusercontent.com/stilesb/dotfiles/master/vim.png)

Vim

![Screenshot](https://raw.githubusercontent.com/stilesb/dotfiles/master/emacs.png)

Emacs

**An opinionated OSX/NixOs/Ubuntu terminal-based software development toolset and workflow**

## Setup

*Requirements*

* iTerm
* Vagrant

To get started on OSX, simply run:

```
vagrant up
```

If you are currently running Ubuntu or a similar Debian distribution, run:

```
sudo curl https://civiclabsconsulting.github.io/dotfiles/install | sh
```

### Keyboard

* Switch `<Control>` and `<Caps Lock>` keys for easier keyboard ergonomics with Vim.

## Tools

### Tmux

Terminal multiplexer. See `tmux.conf` for full configuration details.

* Prefix is `<Control>` + `<a>`

### z

Use `z` to `cd` into commonly used directories.

### Neovim

Like Vim but with better asynchronous support and default configurations.

### Emacs

For use with Coq and Proof General
