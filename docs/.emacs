; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(display-time-mode)

; Packages
(require 'package)

; Not used
(setq package-list '())

; Add Melpa as the default Emacs Package repository
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

; Activate all the packages (in particular autoloads)
(package-initialize)

; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

; Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; Window apperance
(set-default-font "Fira Code Retina 14")
(when window-system
  (set-frame-position (selected-frame) 206 42)
  (set-frame-size (selected-frame) 186 53))

; Side-effects from package-install
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages (quote (flycheck-haskell magit pg helm intero dashboard))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; org-mode
(require 'org)

; smart-mode-line
(require 'smart-mode-line)
(sml/setup)
(setq sml/theme 'light)
(setq sml/mode-width 0)
(setq sml/name-width 20)
(setf rm-blacklist "")

; helm
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)

; magit
(require 'magit)

; dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)

; Proof General
(setq coq-prog-name "/usr/local/bin/coqtop")
(load "~/.emacs.d/lisp/PG/generic/proof-site")

; FP
(require 'haskell-mode)
(require 'intero)
(require 'flycheck-haskell)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
