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
         '(flatui-theme xterm-color dumb-jump ag grep+ ack helm-ag hungry-delete sane-term autopair jsx-mode git-gutter-fringe swiper-helm nginx-mode iedit solarized-theme undo-tree goto-chg nix-mode dockerfile-mode docker org-ac auto-complete smart-mode-line yaml-mode web-mode shakespeare-mode s pg org-journal org-bullets org-agenda-property neotree markdown-mode magit intero helm-projectile helm-flycheck helm-descbinds gruvbox-theme flycheck-haskell rjsx-mode dashboard color-theme base16-theme))
        (refreshed? nil))
    (dolist (p my-packages)
      (unless (package-installed-p p)
        (when (null refreshed?)
          (package-refresh-contents)
          (setq refreshed? t))
        (package-install p)))))
(load-packages)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coq-prog-args (quote ("-R" "/Users/brandon/Code/cpdt/src" "Cpdt")))
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default))))

; System: Path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/brandon/.local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Users/brandon/.local/bin")))

; System: Term
(when t (setenv "TERM" "eterm-color"))
(eval-after-load "term"
  '(progn
     (define-key term-raw-map (kbd "<C-left>")
       (lambda () (interactive) (term-send-raw-string "\eb")))
     (define-key term-raw-map (kbd "<M-left>")
       (lambda () (interactive) (term-send-raw-string "\eb")))
     (define-key term-raw-map (kbd "<C-right>")
       (lambda () (interactive) (term-send-raw-string "\ef")))
     (define-key term-raw-map (kbd "<M-right>")
       (lambda () (interactive) (term-send-raw-string "\ef")))
     (mapc
      (lambda (func)
        (eval `(define-key term-raw-map [remap ,func]
                 (lambda () (interactive) (ding)))))
      '(backward-kill-paragraph backward-kill-sentence backward-kill-sexp backward-kill-word bookmark-kill-line kill-backward-chars kill-backward-up-list kill-forward-chars kill-line kill-paragraph kill-rectangle kill-region kill-sentence kill-sexp kill-visual-line kill-whole-line kill-word subword-backward-kill subword-kill yank yank-pop yank-rectangle))))

; Util: Proof General
(setq coq-prog-name "/usr/local/bin/coqtop")
(when nil (load "~/.emacs.d/lisp/PG/generic/proof-site"))

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
(add-hook 'term-mode-hook (lambda () (define-key term-raw-map (kbd "C-p")
				       (lambda () (interactive) (term-line-mode) (yank) (term-char-mode)))))

; UI: Terminal
(require 'xterm-color)

; Util: Writing
(require 'org)

(setq org-startup-truncated nil)
(setq org-log-done t)
(setq org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELLED")))
      
(with-eval-after-load 'org (add-hook 'org-mode-hook #'visual-line-mode))

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)

; UI: Bullets
(require 'org-bullets)
(setq org-ellipsis " …")
(setq org-bullets-bullet-list '("•"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

; Util: Agenda
(require 'org-agenda)

(global-set-key (kbd "C-c a") 'org-agenda)
(load-library "find-lisp")

(setq org-agenda-files (find-lisp-find-files "~/Dropbox (Personal)/.org" "\.org$"))
(setq org-agenda-compact-blocks t)
(setq org-agenda-start-on-weekday 0)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done 'canceled))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-ndays 1)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (org-skip-subtree-if-habit)
                                                   (org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "All normal-priority tasks:"))))
         ((org-agenda-compact-blocks t)))))

(defun org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

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
(global-set-key (kbd "C-c j") 'org-journal-new-entry)

; Util: Writing encryption
(require 'org-crypt)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "stilesbr1@gmail.com")
(org-crypt-use-before-save-magic)
 
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
    (progn (require 'helm-ag)
	   (global-set-key (kbd "C-x C-/") #'helm-projectile-ag)) ; Not working
    (progn (global-set-key (kbd "C-x C-/") #'helm-projectile-ack)))

; Util: Backspace
(require 'hungry-delete)
(global-hungry-delete-mode)

; UI: Line numbers
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format " %d ")

; UI: Navigation
(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'nerd))
(setq projectile-switch-project-action 'neotree-projectile-action)
;(global-unset-key (kbd "C-x C-n"))
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
(global-set-key (kbd "C-x j") 'dumb-jump-go)

; UI: Git Gutter Fringe
(require 'git-gutter-fringe)

; UI: Nix
(require 'nix-mode)

; UI: Syntax
(require 'nginx-mode)

; UI: Themes
(defvar current-theme 3)
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
				   (load-theme 'solarized-light t))))

; Util: Completion
(require 'swiper)
(require 'swiper-helm)

; Util: Spelling
(defun flyspell-add-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)    
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(global-set-key (kbd "C-c d") 'flyspell-add-word)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
