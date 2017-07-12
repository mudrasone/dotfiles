(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(require 'org)
(setq org-src-fontify-natively t)
(org-babel-load-file (expand-file-name "settings.org" "~/.emacs.d/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-terminal-cursor-changer ack helm-config rainbow-mode epa-file org-crypt org-agenda yaml-mode web-mode web-beautify use-package solidity-mode smart-mode-line shakespeare-mode scala-mode sane-term pg org-journal org-bullets org-agenda-property org-ac nix-mode nginx-mode neotree markdown-mode magit jsx-mode intero iedit hungry-delete helm-projectile helm-flycheck helm-descbinds helm-ag gruvbox-theme grep+ git-gutter-fringe evil-org evil-leader dumb-jump dockerfile-mode docker dashboard ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-dir-link-face ((t (:foreground "#FB4934"))))
 '(neo-file-link-face ((t (:foreground "#FAF4C1")))))
