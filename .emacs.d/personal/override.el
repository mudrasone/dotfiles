(message "Overriding...")

(when window-system
  (setq frame-resize-pixelwise t)
  (set-default-font "Hasklig")
  (set-face-attribute 'default nil :height 150)
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
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

(disable-theme 'zenburn)
(setq prelude-theme nil)

(require 'gruvbox)
(load-theme 'gruvbox)

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx
      interprogram-paste-function 'copy-from-osx)

(require 'org-journal)
(setq org-journal-dir "/Volumes/Turing/Dropbox/.org/journal/"
      org-journal-file-format "%Y%m%d.org")

(require 'org-bullets)
(setq org-bullets-bullet-list '("•"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-agenda-property)
(load-library "find-lisp")
(setq org-agenda-files (find-lisp-find-files "/Volumes/Turing/Dropbox/.org" "\.org$")
      org-agenda-compact-blocks t
      org-agenda-start-on-weekday 0
      org-agenda-property-list '("DEADLINE" "SCHEDULED")
      org-agenda-window-setup (quote current-window)
      org-agenda-span (quote fortnight)
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
      org-agenda-todo-ignore-deadlines (quote all)
      org-agenda-todo-ignore-scheduled (quote all)
      org-agenda-sorting-strategy (quote ((agenda deadline-up priority-down)
                                          (todo priority-down category-keep)
                                          (tags priority-down category-keep)
                                          (search category-keep)))
      org-deadline-warning-days 7
      org-log-done t
      org-agenda-skip-scheduled-if-done t
      org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELLED")))

;; (require 'org-epa)
(epa-file-enable)

;; (require 'org-crypt)
(setq org-tags-exclude-from-inheritance (quote ("crypt"))
      org-crypt-key "stilesbr1@gmail.com")
(org-crypt-use-before-save-magic)

(require 'dashboard)
(setq dashboard-items '((recents  . 10) (bookmarks . 10) (projects . 10)))
(dashboard-setup-startup-hook)

(add-hook 'org-mode-hook (function (lambda () (whitespace-mode -1))))

(require 'neotree)
(setq-default neo-show-hidden-files t)
(setq neo-theme (if (display-graphic-p) 'nerd)
      projectile-switch-project-action 'neotree-projectile-action
      neo-smart-open t)
(add-hook 'neotree-mode-hook (lambda () (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                               (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
                               (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                               (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
(global-set-key (kbd "C-x n") 'neotree-projectile-action)
(custom-set-faces '(neo-dir-link-face ((t (:foreground "#FB4934"))))
                  '(neo-file-link-face ((t (:foreground "#FAF4C1")))))

(require 'web-beautify)
(eval-after-load 'js2-mode '(define-key js2-mode-map (kbd "C-c C-b") 'web-beautify-js))
(eval-after-load 'js '(define-key js-mode-map (kbd "C-c C-b") 'web-beautify-js))
(eval-after-load 'json-mode '(define-key json-mode-map (kbd "C-c C-b") 'web-beautify-js))
(eval-after-load 'sgml-mode '(define-key html-mode-map (kbd "C-c C-b") 'web-beautify-html))
(eval-after-load 'web-mode '(define-key web-mode-map (kbd "C-c C-b") 'web-beautify-html))
(eval-after-load 'css-mode '(define-key css-mode-map (kbd "C-c C-b") 'web-beautify-css))

(require 'flycheck-mypy)
(add-hook 'python-mode-hook 'flycheck-mode)

(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)

(require 'pg)
(setq coq-prog-name "/usr/local/bin/coqtop")
