; Ergonomics: Rebind =<M-x>=
(global-set-key "\C-x\C-m" 'execute-extended-command)

; UI: Centered minimal window
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
 '(coq-prog-args (quote ("-R" "/Users/brandon/Code/cpdt/src" "Cpdt")))
 '(custom-safe-themes
   (quote
    ("cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "25c242b3c808f38b0389879b9cba325fb1fa81a0a5e61ac7cae8da9a32e2811b" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "4605ce6e798971d215b01844ea39e993d683aa2fa118e02e263539298f9f3921" "a85e40c7d2df4a5e993742929dfd903899b66a667547f740872797198778d7b5" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" "60e09d2e58343186a59d9ed52a9b13d822a174b33f20bdc1d4abb86e6b17f45b" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "fee4e306d9070a55dce4d8e9d92d28bd9efe92625d2ba9d4d654fc9cd8113b7f" "50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" "d83e34e28680f2ed99fe50fea79f441ca3fddd90167a72b796455e791c90dc49" "100eeb65d336e3d8f419c0f09170f9fd30f688849c5e60a801a1e6addd8216cb" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(package-selected-packages
   (quote
    (undo-tree goto-chg nix-mode dockerfile-mode docker org-ac auto-complete smart-mode-line yaml-mode web-mode shakespeare-mode sane-term s pg org-journal org-bullets org-agenda-property neotree markdown-mode magit intero helm-projectile helm-flycheck helm-descbinds helm-ag gruvbox-theme flycheck-haskell dashboard color-theme base16-theme))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; Util: Terminal
(require 'sane-term)
(setq sane-term-shell-command "/bin/bash")
(global-set-key (kbd "C-x t") 'sane-term)
(global-set-key (kbd "C-x T") 'sane-term-create)

; Optional convenience binding. This allows C-y to paste even when in term-char-mode
(add-hook 'term-mode-hook
	  (lambda() (define-key term-raw-map (kbd "C-y")
		      (lambda () (interactive)
			(term-line-mode) (yank) (term-char-mode)))))

; Util: Writing and organization
(require 'org)
(setq org-log-done t)
(setq org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELLED")))
(setq org-startup-truncated nil)
(setq org-agenda-start-on-weekday 0) 
(with-eval-after-load 'org (add-hook 'org-mode-hook #'visual-line-mode))
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)

(require 'org-crypt)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "stilesbr1@gmail.com")
(org-crypt-use-before-save-magic)

(require 'org-bullets)
(setq org-ellipsis " …")
(setq org-bullets-bullet-list '("•"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-agenda)
(load-library "find-lisp")
(setq org-agenda-files
      (find-lisp-find-files "~/Dropbox (Personal)/.org" "\.org$"))
(global-set-key (kbd "C-c a") 'org-agenda)

(require 'org-agenda-property)
(setq org-agenda-property-list '("DEADLINE" "SCHEDULED"))

(require 'org-journal)
(setq org-journal-dir "~/Dropbox (Personal)/.org/journal/")
(setq org-journal-file-format "%Y%m%d.org")
(add-hook 'org-journal-mode-hook 'org-mode)
(global-set-key (kbd "C-c j") 'org-journal-new-entry)
 
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
(load-theme 'base16-tomorrow)
(set-face-background 'fringe "white")
;(load-theme 'gruvbox t)

; UI: Dashboard
(require 'dashboard)
(setq dashboard-items '((recents  . 15)
                        (bookmarks . 15)
			(projects . 15)))
(dashboard-setup-startup-hook)

; Util: Web development
(require 'web-mode)

; Util: Encryption
(require 'epa-file)
(epa-file-enable)

; UI: Smart Mode Lin
(require 'smart-mode-line)
(setq sml/theme 'respectful)
(sml/setup)

; Util: Autocomplete
(require 'auto-complete)
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'org-mode 'markdown-mode)

(require 'org-ac)
(org-ac/config-default)

; Util: Writing
(require 'markdown-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)

; Util: Navigation
(require 'goto-chg)

; Util: Undo
(require 'undo-tree)

(when (window-system)
  (set-default-font "Hasklig"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))
(set-face-attribute 'default nil :height 140)
