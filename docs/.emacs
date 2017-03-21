; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

; Meta key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

; Packages
(require 'package)
(setq package-list '())
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; Window
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
    ("d5b121d69e48e0f2a84c8e4580f0ba230423391a78fcb4001ccb35d02494d79e" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (multi-term blackboard-theme hc-zenburn-theme helm-flycheck org-journal flycheck-haskell magit pg helm intero dashboard))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; org
(require 'org)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

; org-journal
(require 'org-journal)
(setq org-agenda-files '("~/Documents/journal"))
(setq org-agenda-file-regexp "\\`[^.].*\\.org\\'\\|[0-9]+")

; smart-mode-line
(require 'smart-mode-line)
(sml/setup)
(setq sml/theme 'light)

; theme
;(load-theme 'wombat)

; helm
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)

; helm-flycheck
(require 'helm-flycheck)

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
(add-hook 'haskell-mode-hook 'intero-mode)

; Path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

; System
(require 'multi-term)
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term)
(setq multi-term-program "/bin/bash")
