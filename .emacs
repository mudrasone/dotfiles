; Ergonomics: Rebind =<M-x>=
(global-set-key "\C-x\C-m" 'execute-extended-command)

; UI: Minimal window
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (set-frame-position (selected-frame) 140 42)
  (set-frame-size (selected-frame) 180 50))
(setq frame-resize-pixelwise t)
      
; Util: Proof General
(setq coq-prog-name "/usr/local/bin/coqtop")
(load "~/.emacs.d/lisp/PG/generic/proof-site")

; UI: Font
(when (window-system)
  (set-default-font "Hasklig"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
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
(set-face-attribute 'default nil :height 150)

; System: Path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/brandon/.local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Users/brandon/.local/bin")))

; System: Term
(when nil (setenv "TERM" "eterm-color"))
(eval-after-load "term"
  '(progn
     ;; Fix forward/backward word when (term-in-char-mode).
     (define-key term-raw-map (kbd "<C-left>")
       (lambda () (interactive) (term-send-raw-string "\eb")))
     (define-key term-raw-map (kbd "<M-left>")
       (lambda () (interactive) (term-send-raw-string "\eb")))
     (define-key term-raw-map (kbd "<C-right>")
       (lambda () (interactive) (term-send-raw-string "\ef")))
     (define-key term-raw-map (kbd "<M-right>")
       (lambda () (interactive) (term-send-raw-string "\ef")))
     ;; Disable killing and yanking in char mode (term-raw-map).
     (mapc
      (lambda (func)
        (eval `(define-key term-raw-map [remap ,func]
                 (lambda () (interactive) (ding)))))
      '(backward-kill-paragraph
        backward-kill-sentence backward-kill-sexp backward-kill-word
        bookmark-kill-line kill-backward-chars kill-backward-up-list
        kill-forward-chars kill-line kill-paragraph kill-rectangle
        kill-region kill-sentence kill-sexp kill-visual-line
        kill-whole-line kill-word subword-backward-kill subword-kill
        yank yank-pop yank-rectangle))))

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
    ("15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "ea489f6710a3da0738e7dbdfc124df06a4e3ae82f191ce66c2af3e0a15e99b90" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "92192ea8f0bf04421f5b245d906701abaa7bb3b0d2b3b14fca2ee5ebb1da38d8" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "dd1cf47034b1c20f5f43cd91ae76f00abef05f91b7be57d94653c493bcf41dda" "87233846530d0b2c50774c74c4aca06a1472504c63ccd4ab2b1021b3e56a69e9" "ad16a1bf1fd86bfbedae4b32c269b19f8d20d416bd52a87cd50e355bf13c2f23" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "25c242b3c808f38b0389879b9cba325fb1fa81a0a5e61ac7cae8da9a32e2811b" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "4605ce6e798971d215b01844ea39e993d683aa2fa118e02e263539298f9f3921" "a85e40c7d2df4a5e993742929dfd903899b66a667547f740872797198778d7b5" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" "60e09d2e58343186a59d9ed52a9b13d822a174b33f20bdc1d4abb86e6b17f45b" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "fee4e306d9070a55dce4d8e9d92d28bd9efe92625d2ba9d4d654fc9cd8113b7f" "50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" "d83e34e28680f2ed99fe50fea79f441ca3fddd90167a72b796455e791c90dc49" "100eeb65d336e3d8f419c0f09170f9fd30f688849c5e60a801a1e6addd8216cb" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(package-selected-packages
   (quote
    (rjsx-mode flatui-theme xterm-color dumb-jump ag grep+ ack helm-ag hungry-delete sane-term autopair jsx-mode git-gutter-fringe swiper-helm nginx-mode iedit solarized-theme undo-tree goto-chg nix-mode dockerfile-mode docker org-ac auto-complete smart-mode-line yaml-mode web-mode shakespeare-mode s pg org-journal org-bullets org-agenda-property neotree markdown-mode magit intero helm-projectile helm-flycheck helm-descbinds gruvbox-theme flycheck-haskell dashboard color-theme base16-theme))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; Util: Pair
(require 'autopair)
(autopair-global-mode)

; Util: Terminal
(require 'sane-term)
(global-set-key (kbd "C-x t") 'sane-term)
(global-set-key (kbd "C-x T") 'sane-term-create)
(setq term-term-name "xterm")
(setq sane-term-shell-command "/bin/bash")
(add-hook 'term-mode-hook (lambda () (setq term-buffer-maximum-size 10000)))
(add-hook 'term-mode-hook
	  (lambda() (define-key term-raw-map (kbd "C-y")
		      (lambda () (interactive)
			(term-line-mode) (yank) (term-char-mode)))))

; UI: Terminal
(require 'xterm-color)

; Util: Writing
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

; UI: Bullets
(require 'org-bullets)
(setq org-ellipsis " …")
(setq org-bullets-bullet-list '("•"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

; Util: Agenda
(require 'org-agenda)
(load-library "find-lisp")
(setq org-agenda-files
      (find-lisp-find-files "~/Dropbox (Personal)/.org" "\.org$"))
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-compact-blocks t)
(defun my/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))
(add-hook 'org-mode-hook 'my/org-mode-hook)

; Util: Agenda properties
(require 'org-agenda-property)
(setq org-agenda-property-list '("DEADLINE" "SCHEDULED"))

; Util: Journal
(require 'org-journal)
(setq org-journal-dir "~/Dropbox (Personal)/.org/journal/")
(setq org-journal-file-format "%Y%m%d.org")
(add-hook 'org-journal-mode-hook 'org-mode)
(global-set-key (kbd "C-c j") 'org-journal-new-entry)
 
; Util: Project management
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

; UI: Line numbers
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format " %d ")

; Util: Helm Projectile
(require 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(global-set-key (kbd "C-x C-p") 'helm-projectile-find-file)

; Util: Helm help
(require 'helm-descbinds)
(helm-descbinds-mode)

; Util: Helm auto-complete
(require 'helm-flycheck)

; Util: Helm Ag
(if nil
    ;; Not working
    (progn (require 'helm-ag)
	   (global-set-key (kbd "C-x C-/") #'helm-projectile-ag))
    (progn (global-set-key (kbd "C-x C-/") #'helm-projectile-ack)))

; Util: Backspace
(require 'hungry-delete)
(global-hungry-delete-mode)

; UI: Navigation
(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'nerd))
(setq projectile-switch-project-action 'neotree-projectile-action)
(global-unset-key (kbd "C-x C-n"))
(global-set-key (kbd "C-x C-n") 'neotree-toggle)

; UI: Haskell
(require 'haskell-mode)

; Util: Haskell
(require 'flycheck-haskell)

; Util: Intero
(require 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)

; Util: Magit
(require 'magit)

; UI: Yaml
(require 'yaml-mode)

; UI: Dashboard
(require 'dashboard)
(setq dashboard-items '((recents  . 5) (bookmarks . 5) (projects . 5)))
(dashboard-setup-startup-hook)

; Util: Web development
(require 'web-mode)

; Util: Encryption
(require 'epa-file)
(epa-file-enable)

; Util: Writing encryption
(require 'org-crypt)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "stilesbr1@gmail.com")
(org-crypt-use-before-save-magic)

; UI: Smart Mode Line
(require 'smart-mode-line)
(setq sml/theme 'respectful)
(sml/setup)

; Util: Auto-complete
(require 'auto-complete)
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'org-mode 'markdown-mode)

; Util: Org auto-complete
(require 'org-ac)
(org-ac/config-default)

; Util: Org habits
(require 'org-habit)

; Util: Writing
(require 'markdown-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)

; Util: Navigation
(require 'goto-chg)

; Util: Undo
(require 'undo-tree)

; Util: Edit multiple text string occurances at once
(require 'iedit)

; Util: Navigation
(require 'dumb-jump)
(setq dumb-jump-selector 'helm)
(global-set-key (kbd "C-x C-g") 'dumb-jump-go)

; Util: Completion
(require 'swiper)
(require 'swiper-helm)

; Util: Git Gutter Fringe
(require 'git-gutter-fringe)

; Util: Nix
(require 'nix-mode)

					; UI: Syntax
(require 'nginx-mode)

; UI: Themes
(cond (nil (progn (load-theme 'base16-tomorrow t)
                  (set-face-background 'fringe "white")))
      (nil   (progn (require 'flatui-theme)
	    	    (load-theme 'flatui t)
 		    (global-linum-mode 1)
		    (set-face-background 'linum "#ecf0f1")
		    (set-face-background 'fringe "#ecf0f1")
		    (global-linum-mode 0)))
      (nil (load-theme 'gruvbox t))
      (t (require 'solarized-theme)
	 (setq x-underline-at-descent-line t)
         (setq solarized-high-contrat-mode-line nil)
         (setq solarized-use-variable-pitch nil)
         (setq solarized-scale-org-headlines nil)
         (load-theme 'solarized-light t)))

; Util: Spelling
(defun flyspell-add-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)    
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))
(global-set-key (kbd "C-c s") 'flyspell-add-word)
(put 'erase-buffer 'disabled nil)
