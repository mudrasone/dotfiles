; Packages: Setup
(require 'package)

(defun load-packages ()
  (package-initialize)
  (setq package-archives
        '(("marmalade" . "http://marmalade-repo.org/packages/")
          ("gnu" . "http://elpa.gnu.org/packages/")
          ("org" . "http://orgmode.org/elpa/")
          ("melpa" . "https://melpa.org/packages/")
          ("melpa-stable" . "https://stable.melpa.org/packages/")))
  (let ((my-packages
         '(flatui-theme scala-mode evil evil-leader xterm-color dumb-jump ag grep+ ack helm-ag hungry-delete sane-term jsx-mode web-beautify git-gutter-fringe nginx-mode iedit solarized-theme undo-tree goto-chg nix-mode dockerfile-mode docker org-ac auto-complete smart-mode-line yaml-mode web-mode shakespeare-mode s pg org-journal org-bullets org-agenda-property neotree solidity-mode rainbow-mode markdown-mode magit intero helm-projectile helm-flycheck helm-descbinds gruvbox-theme dashboard color-theme base16-theme))
        (refreshed? nil))
    (dolist (p my-packages)
      (unless (package-installed-p p)
        (when (null refreshed?)
          (package-refresh-contents)
          (setq refreshed? t))
        (package-install p)))))

(load-packages)

; Ergonomics: Rebind <M-x> to <C-x X-m>
(global-set-key "\C-x\C-m" 'execute-extended-command)

; UI: Minimal window
(when window-system
  (setq frame-resize-pixelwise t)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (set-frame-position (selected-frame) 140 42)
  (set-frame-size (selected-frame) 180 50)
  (set-default-font "Hasklig")
  (set-face-attribute 'default nil :height 150))

; UI: Font ligatures
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
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

; Util: Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; System: Path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/brandon/.local/bin:/Users/brandon/.nvm/versions/node/v6.4.0/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Users/brandon/.local/bin")))
(setq exec-path (append exec-path '("/Users/brandon/.nvm/versions/node/v6.4.0/bin")))

; System: Term
(require 'xterm-color)
(when nil (setenv "TERM" "xterm-256color"))
(when t (setenv "TERM" "eterm-color"))

; Util: Terminal
(require 'sane-term)
(setq term-term-name "xterm")
(setq sane-term-shell-command "/bin/bash")
(add-hook 'term-mode-hook (lambda () (setq term-buffer-maximum-size 10000)))

; Util: Proof General
(setq coq-prog-name "/usr/local/bin/coqtop")
(when nil (load "~/.emacs.d/lisp/PG/generic/proof-site"))

; Util: Writing
(require 'org)

(setq org-startup-truncated nil)
(setq org-log-done t)
(setq org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELLED")))
      
(with-eval-after-load 'org (add-hook 'org-mode-hook #'visual-line-mode))

; UI: Dashboard
(require 'dashboard)
(setq dashboard-items '((recents  . 5) (bookmarks . 5) (projects . 5)))
(dashboard-setup-startup-hook)

; UI: Bullets
(require 'org-bullets)
(setq org-ellipsis " …")
(setq org-bullets-bullet-list '("•"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

; Util: Agenda
(require 'org-agenda)
(load-library "find-lisp")
(setq org-agenda-files (find-lisp-find-files "~/Dropbox (Personal)/.org" "\.org$"))
(setq org-agenda-compact-blocks t)
(setq org-agenda-start-on-weekday 0)
(setq org-agenda-skip-scheduled-if-done t)

(defun org-mode-header-hook ()
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))
(add-hook 'org-mode-hook 'org-mode-header-hook)

; Util: Agenda properties
(require 'org-agenda-property)
(setq org-agenda-property-list '("DEADLINE" "SCHEDULED"))
(setq org-agenda-window-setup (quote current-window))
(setq org-deadline-warning-days 7)
(setq org-agenda-span (quote fortnight))
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))
(setq org-agenda-sorting-strategy
  (quote
   ((agenda deadline-up priority-down)
    (todo priority-down category-keep)
    (tags priority-down category-keep)
    (search category-keep))))

; Util: Journal
(require 'org-journal)
(setq org-journal-dir "~/Dropbox (Personal)/.org/journal/")
(setq org-journal-file-format "%Y%m%d.org")
(add-hook 'org-journal-mode-hook 'org-mode)

; Util: Org auto-complete
(require 'org-ac)
(org-ac/config-default)

; Util: Writing encryption
(require 'org-crypt)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "stilesbr1@gmail.com")
(org-crypt-use-before-save-magic)
 
; Util: Encryption
(require 'epa-file)
(epa-file-enable)

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
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)
(helm-mode 1)

; Util: Helm Projectile
(require 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

; Util: Helm help
(require 'helm-descbinds)
(helm-descbinds-mode)

; Util: Backspace
(require 'hungry-delete)
(global-hungry-delete-mode)

; UI: Neotree
(require 'neotree)
(setq-default neo-show-hidden-files t)
(setq neo-theme (if (display-graphic-p) 'nerd))
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq neo-smart-open t)
(add-hook 'neotree-mode-hook
	  (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

; Util: Haskell development
(require 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)

; Util: Web development
(require 'web-beautify)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coq-prog-args (quote ("-R" "/Users/brandon/Code/cpdt/src" "Cpdt")))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default))))
(custom-set-faces
 '(neo-dir-link-face ((t (:foreground "#278BD2"))))
 '(neo-file-link-face ((t (:foreground "#657B84"))))
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; UI: Smart Mode Line
(require 'smart-mode-line)
(setq sml/theme 'respectful)
(sml/setup)

; Util: Auto-complete
(require 'auto-complete)
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'org-mode 'markdown-mode)

; Util: Navigation
(require 'goto-chg)

; Util: Undo
(require 'undo-tree)

; Util: Edit multiple text string occurances at once
(require 'iedit)

; Util: Navigation
(require 'dumb-jump)
(setq dumb-jump-selector 'helm)

; UI: VCS
(require 'magit)
(require 'git-gutter-fringe)

; UI: Modes
(require 'nix-mode)
(require 'nginx-mode)
(require 'rainbow-mode)
(require 'web-mode)
(require 'yaml-mode)
(require 'solidity-mode)
(require 'haskell-mode)
(require 'markdown-mode)
(require 'scala-mode)

(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

; UI: Themes
(defvar current-theme 5)
(cond ((eq current-theme 0) (progn (load-theme 'base16-tomorrow t)
				   (set-face-background 'fringe "#eeeeee")))
      ((eq current-theme 1) (progn (require 'flatui-theme)
				   (load-theme 'flatui t)
				   (global-linum-mode 1)
				   (set-face-background 'linum "#ecf0f1")
				   (set-face-background 'fringe "#ecf0f1")
				   (global-linum-mode 0)))
      ((eq current-theme 2) (load-theme 'gruvbox t))
      ((eq current-theme 3) (progn (setq solarized-use-variable-pitch nil)
				   (setq solarized-scale-org-headlines nil)
				   (setq solarized-height-minus-1 1.0)
				   (setq solarized-height-plus-1 1.0)
				   (setq solarized-height-plus-2 1.0)
				   (setq solarized-height-plus-3 1.0)
				   (setq solarized-height-plus-4 1.0)
				   (setq x-underline-at-descent-line t)
				   (setq solarized-high-contrat-mode-line nil)
				   (require 'solarized-theme)
				   (load-theme 'solarized-dark t)))
      ((eq current-theme 4) (progn (require 'zenburn-theme)
                                   (load-theme 'zenburn)))
      ((eq current-theme 5) (progn (load-theme 'solarized)
				   (set-face-background 'fringe "#FDF6E3")))
      ((eq current-theme 6) (progn (require 'firebelly-theme)
                                   (load-theme 'firebelly))))

; Util: Spelling
(require 'flyspell)

(defun flyspell-add-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)    
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

; UI: Line numbers
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format " %d ")

; Util: Evil
(setq evil-want-C-u-scroll t)
(setq evil-leader/in-all-states t)

(require 'evil)
(require 'evil-leader)

(evil-mode 1)
(global-evil-leader-mode 1)
(evil-leader/set-leader "<SPC>")

(global-set-key (kbd "C-c w") 'flyspell-add-word)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c j") 'org-journal-new-entry)

(global-set-key (kbd "C-x d") 'dumb-jump-go)
(global-set-key (kbd "C-x b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x f") 'helm-find-files)

(define-key evil-insert-state-map (kbd "C-n") 'neotree-toggle)
(define-key evil-normal-state-map (kbd "C-n") 'neotree-toggle)
(define-key evil-visual-state-map (kbd "C-n") 'neotree-toggle)
(define-key evil-motion-state-map (kbd "C-n") 'neotree-toggle)

(define-key evil-insert-state-map (kbd "C-/") 'helm-projectile-ack)
(define-key evil-normal-state-map (kbd "C-/") 'helm-projectile-ack)
(define-key evil-visual-state-map (kbd "C-/") 'helm-projectile-ack)
(define-key evil-motion-state-map (kbd "C-/") 'helm-projectile-ack)

(define-key evil-insert-state-map (kbd "C-f") 'helm-projectile-find-file)
(define-key evil-normal-state-map (kbd "C-f") 'helm-projectile-find-file)
(define-key evil-visual-state-map (kbd "C-f") 'helm-projectile-find-file)
(define-key evil-motion-state-map (kbd "C-f") 'helm-projectile-find-file)

(define-key evil-insert-state-map (kbd "C-b") 'switch-to-buffer)
(define-key evil-normal-state-map (kbd "C-b") 'switch-to-buffer)
(define-key evil-visual-state-map (kbd "C-b") 'switch-to-buffer)
(define-key evil-motion-state-map (kbd "C-b") 'switch-to-buffer)

(define-key evil-insert-state-map (kbd "C-t") 'sane-term)
(define-key evil-normal-state-map (kbd "C-t") 'sane-term)
(define-key evil-visual-state-map (kbd "C-t") 'sane-term)
(define-key evil-motion-state-map (kbd "C-t") 'sane-term)

(define-key evil-insert-state-map (kbd "C-s") 'sane-term-create)
(define-key evil-normal-state-map (kbd "C-s") 'sane-term-create)
(define-key evil-visual-state-map (kbd "C-s") 'sane-term-create)
(define-key evil-motion-state-map (kbd "C-s") 'sane-term-create)

(evil-define-key 'normal term-raw-map "p" 'term-paste)
(fset 'evil-visual-update-x-selection 'ignore)

(eval-after-load 'js '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'json-mode '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'sgml-mode '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'web-mode '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'css-mode '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))
(eval-after-load 'haskell-mode '(define-key haskell-mode-map (kbd "C-c b") 'haskell-mode-stylish-buffer))
