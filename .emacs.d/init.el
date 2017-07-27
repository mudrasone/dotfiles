(require 'package)
(add-to-list 'package-archives '("melpa" .   "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(require 'org)
(org-babel-load-file (expand-file-name "settings.org" "~/.emacs.d/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-terminal-cursor-changer evil-org evil-leader evil hlinum ack goto-chg undo-tree iedit hungry-delete ag dumb-jump neotree helm-ag helm-descbinds helm-flycheck helm-projectile projectile helm-config dashboard gruvbox-theme intero scala-mode smart-mode-line magit markdown-mode rainbow-mode solidity-mode shakespeare-mode web-mode yaml-mode docker dockerfile-mode nix-mode nginx-mode web-beautify jsx-mode epa-file org-crypt org-ac org-journal org-agenda-property org-agenda org-bullets pg sane-term use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-dir-link-face ((t (:foreground "#FB4934"))))
 '(neo-file-link-face ((t (:foreground "#FAF4C1")))))
