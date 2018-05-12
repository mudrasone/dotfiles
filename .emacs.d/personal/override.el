;; Packages
(prelude-require-packages '(dashboard
                            solarized-theme
                            solidity-mode
                            nix-mode
                            jsx-mode
                            org-journal
                            org-bullets
                            smart-hungry-delete
                            writeroom-mode
                            elm-mode
                            vue-mode
                            neotree))

;; Dashboard
(setq dashboard-items '((recents  . 10) (bookmarks . 10) (projects . 10)))
(dashboard-setup-startup-hook)

;; Theme
(setq x-underline-at-descent-line t)
(load-theme 'solarized-dark)

;; Neotree
(defun my-neotree-mode-hook ()
  (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
  (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
  (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
  (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))

(defun my-neotree-project-toggle ()
  (interactive)
  (let ((project-dir
         (ignore-errors (projectile-project-root)))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
            (neotree-dir project-dir))
        (if file-name
            (neotree-find file-name))))))

(setq neo-theme (if (display-graphic-p) 'nerd)
      projectile-switch-project-action 'neotree-projectile-action
      neo-show-hidden-files t
      neo-smart-open t)

(add-hook 'neotree-mode-hook 'my-neotree-mode-hook)
(global-set-key (kbd "C-c p n") 'my-neotree-project-toggle)

;; SML
(setq sml/theme 'respectful)

;; Whitespce
(setq prelude-whitespace nil)

;; HL
(hl-line-mode -1)
(global-hl-line-mode -1)
(global-diff-hl-mode -1)

;; Font
(when (and window-system nil) (set-default-font "Hasklig")
      (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                     (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                     (36 . ".\\(?:>\\)")
                     (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                     (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                     (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                     (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                     (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                     ;; (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                     (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                     (48 . ".\\(?:x[a-zA-Z]\\)")
                     (58 . ".\\(?:::\\|[:=]\\)")
                     (59 . ".\\(?:;;\\|;\\)")
                     (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                     (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                     (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                     ;; (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
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
                                `([,(cdr char-regexp) 0 font-shape-gstring])))))
(set-default-font "Source Code Pro")

;; Org Journal
(setq org-journal-dir "~/Dropbox/.org/journal/"
      org-journal-file-format "%Y%m%d.org")

(setq org-bullets-bullet-list '("•"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; C++
(setq auto-mode-alist (append '(("\\.tcc$" . c-mode)) auto-mode-alist))

;; JSX
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))

(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js-indent-level 2)

(let ((checkers (get 'javascript-eslint 'flycheck-next-checkers)))
  (put 'javascript-eslint 'flycheck-next-checkers
       (remove '(warning . javascript-jscs) checkers)))

(defun setup-js2-mode ()
  (flycheck-select-checker 'babel-eslint)
  (flycheck-mode))

(add-hook 'js2-mode-hook #'setup-js2-mode)
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

;; Write room
(global-set-key (kbd "C-c p w") 'writeroom-mode)

;; Backspace
(smart-hungry-delete-add-default-hooks)
(global-set-key (kbd "<backspace>") 'smart-hungry-delete-backward-char)

;; Crypt
(epa-file-enable)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "stilesbr1@gmail.com")

;; Fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Extensions
(add-to-list 'auto-mode-alist '("Dockerfile.*" . dockerfile-mode))

;; Fastlane
(add-to-list 'auto-mode-alist '("Fastfile" . ruby-mode))

;; Vue Mode
(setq mmm-submode-decoration-level 0)

(defun my-correct-symbol-bounds (pretty-alist)
  "Prepend a TAB character to each symbol in this alist,
   this way compose-region called by prettify-symbols-mode
   will use the correct width of the symbols
   instead of the width measured by char-width."
  (mapcar (lambda (el)
            (setcdr el (string ?\t (cdr el)))
            el)
          pretty-alist))

(defun my-ligature-list (ligatures codepoint-start)
  "Create an alist of strings to replace with codepoints starting from 
   codepoint-start."
  (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
    (-zip-pair ligatures codepoints)))

(setq my-hasklig-ligatures
      (let* ((ligs '("&&" "***" "*>" "\\\\" "||" "|>" "::"
                     "==" "===" "==>" "=>" "=<<" "!!" ">>"
                     ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
                     "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
                     "<<" "<<<" "<+>" ".." "..." "++" "+++"
                     "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->")))
        (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))

(defun my-set-hasklig-ligatures ()
  "Add hasklig ligatures for use with prettify-symbols-mode."
  (setq prettify-symbols-alist
        (append my-hasklig-ligatures prettify-symbols-alist))
  (prettify-symbols-mode))

(add-hook 'haskell-mode-hook 'my-set-hasklig-ligatures)
(add-hook 'elm-mode-hook 'my-set-hasklig-ligatures)

;; Spell check
;;
;; if (aspell installed) { use aspell}
;; else if (hunspell installed) { use hunspell }
;; whatever spell checker I use, I always use English dictionary
;; I prefer use aspell because:
;; 1. aspell is older
;; 2. looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(defun flyspell-detect-ispell-args (&optional run-together)
  "if RUN-TOGETHER is true, spell check the CamelCase words."
  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      ;; Force the English dictionary for aspell
      ;; Support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra" "--lang=en_US"))
      (if run-together
          (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
     ((string-match "hunspell$" ispell-program-name)
      ;; Force the English dictionary for hunspell
      (setq args "-d en_US")))
    args))

(cond
 ((executable-find "aspell")
  ;; you may also need `ispell-extra-args'
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")

  ;; Please note that `ispell-local-dictionary` itself will be passed to hunspell cli with "-d"
  ;; it's also used as the key to lookup ispell-local-dictionary-alist
  ;; if we use different dictionary
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
 (t (setq ispell-program-name nil)))

;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
;; Please note when you use hunspell, ispell-extra-args will NOT be used.
;; Hack ispell-local-dictionary-alist instead.
(setq-default ispell-extra-args (flyspell-detect-ispell-args t))
;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defun text-mode-hook-setup ()
  ;; Turn off RUN-TOGETHER option when spell check text-mode
  (setq-local ispell-extra-args (flyspell-detect-ispell-args)))
(add-hook 'text-mode-hook 'text-mode-hook-setup)
