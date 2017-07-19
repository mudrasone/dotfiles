(setq inhibit-startup-message t)

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

(use-package org
 :config 
 (setq org-src-fontify-natively t)
 :init
 (org-babel-load-file (expand-file-name "settings.org" "~/.emacs.d/")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-terminal-cursor-changer evil-org evil-leader evil ack goto-chg undo-tree iedit hungry-delete grep+ ag dumb-jump neotree helm-ag helm-descbinds helm-flycheck helm-projectile helm-config projectile dashboard gruvbox-theme intero scala-mode smart-mode-line magit markdown-mode rainbow-mode solidity-mode shakespeare-mode web-mode yaml-mode docker dockerfile-mode nix-mode nginx-mode git-gutter-fringe web-beautify jsx-mode epa-file org-crypt org-ac org-journal org-agenda-property org-agenda org-bullets pg sane-term use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-dir-link-face ((t (:foreground "#FB4934"))))
 '(neo-file-link-face ((t (:foreground "#FAF4C1")))))
