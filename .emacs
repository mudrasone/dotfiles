; Ergonomics: Rebind =extended-command=
(global-set-key "\C-x\C-m" 'execute-extended-command)

; UI: Minimal window
(set-default-font "Hasklig 14")
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (set-frame-position (selected-frame) 206 42)
  (set-frame-size (selected-frame) 186 52))

; Packages: Setup
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

; Packages: Side-effects from package-install
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(package-selected-packages
   (quote
    (smart-mode-line yaml-mode web-mode shakespeare-mode sane-term s pg org-journal org-bullets org-agenda-property neotree markdown-mode magit intero helm-projectile helm-flycheck helm-descbinds helm-ag gruvbox-theme google flycheck-haskell dashboard color-theme base16-theme))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; Util: Terminal
(require 'sane-term)
(global-set-key (kbd "C-x t") 'sane-term)
(global-set-key (kbd "C-x T") 'sane-term-create)

; Optional convenience binding. This allows C-y to paste even when in term-char-mode (see below). 
(add-hook 'term-mode-hook (lambda() (define-key term-raw-map (kbd "C-y") (lambda () (interactive) (term-line-mode) (yank) (term-char-mode)))))

; Util: Writing and organization
(require 'org)
(setq org-log-done t)
(setq org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELLED")))
(setq org-startup-truncated nil)
(setq org-agenda-start-on-weekday 0)

(with-eval-after-load 'org (add-hook 'org-mode-hook #'visual-line-mode))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "stilesbr1@gmail.com")

(require 'org-bullets)
(setq org-ellipsis " …")
(setq org-bullets-bullet-list '("•"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-agenda)
(setq org-agenda-files '("~/Dropbox (Personal)/.org"
			 "~/Dropbox (Personal)/.org/meetings"
			 "~/Dropbox (Personal)/.org/journal"))
(global-set-key (kbd "C-c a") 'org-agenda)

(require 'org-agenda-property)
(setq org-agenda-property-list '("DEADLINE" "SCHEDULED"))

(require 'org-journal)
(setq org-journal-dir "~/Dropbox (Personal)/.org/journal/")
(setq org-journal-file-format "%Y%m%d.org")

(defun add-word-to-dictionary ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)    
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

; Util: Projectile
(require 'projectile)
(setq projectile-indexing-method 'alien)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching nil)
(projectile-global-mode)

; Util: Helm
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

(helm-mode 1)

(require 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(global-set-key (kbd "C-x C-p") 'helm-projectile-find-file)

(require 'helm-descbinds)
(helm-descbinds-mode)

(require 'helm-flycheck)

(require 'helm-ag)
(global-set-key (kbd "C-x C-/") 'helm-ag)

(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'nerd))
(setq projectile-switch-project-action 'neotree-projectile-action)
(global-unset-key (kbd "C-x C-n"))
(global-set-key (kbd "C-x C-n") 'neotree-toggle)

; Util: Proof General
(setq coq-prog-name "/usr/local/bin/coqtop")
(load "~/.emacs.d/lisp/PG/generic/proof-site")

; Util: Intero and Haskell Mode
(require 'haskell-mode)
(require 'intero)
(require 'flycheck-haskell)
(add-hook 'haskell-mode-hook 'intero-mode)

; Util: Magit
(require 'magit)

; System: PATH
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

; UI: Yaml Mode
(require 'yaml-mode)

; UI: Theme
;(load-theme 'base16-tomorrow)
;(set-face-background 'fringe "white")
(load-theme 'gruvbox t)

; PG: CPDT
;(custom-set-variables '(coq-prog-args '("-R" "/Users/brandon/Code/cpdt/src" "Cpdt")))

; UI: Dashboard
(require 'dashboard)
(setq dashboard-items '((recents  . 10)
                        (bookmarks . 10)
			(projects . 10)))
(dashboard-setup-startup-hook)

(require 'web-mode)

; Util: Encryption
(require 'epa-file)
(epa-file-enable)

(require 'smart-mode-line)
(setq sml/theme 'respectful)
(sml/setup)
