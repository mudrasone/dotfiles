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
    ("3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" "9955cc54cc64d6c051616dce7050c1ba34efc2b0613d89a70a68328f34e22c8f" "a62f0662e6aa7b05d0b4493a8e245ab31492765561b08192df61c9d1c7e1ddee" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "92192ea8f0bf04421f5b245d906701abaa7bb3b0d2b3b14fca2ee5ebb1da38d8" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "d5b121d69e48e0f2a84c8e4580f0ba230423391a78fcb4001ccb35d02494d79e" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (markdown-mode org-agenda-property shakespeare-mode helm-ag sane-term helm-projectile base16-theme yaml-mode projectile org-bullets helm-descbinds helm-flycheck org-journal flycheck-haskell magit pg helm intero dashboard))))
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

; UI: Smart Mode Line
(require 'smart-mode-line)
(sml/setup)
(setq sml/theme 'respectful)

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

; PG: CPDT
;(custom-set-variables '(coq-prog-args '("-R" "/Users/brandon/Code/cpdt/src" "Cpdt")))

; UI: Dashboard
(require 'dashboard)
(setq dashboard-items '((recents  . 10)
                        (bookmarks . 10)
			(projects . 10)))
(dashboard-setup-startup-hook)

; Util: Encryption
(require 'epa-file)
  (epa-file-enable)
