;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'org)
(setq org-src-fontify-natively t)
(org-babel-load-file (expand-file-name "settings.org" "~/.emacs.d/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coq-prog-args (quote ("-R" "/Users/brandon/Code/cpdt/src" "Cpdt")))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(package-selected-packages
   (quote
    (intero yaml-mode xterm-color web-mode web-beautify use-package swiper-helm spaceline solidity-mode solarized-theme smart-mode-line shakespeare-mode scala-mode sane-term rainbow-mode pg org-journal org-bullets org-agenda-property org-ac nix-mode nginx-mode neotree markdown-mode magit jsx-mode iedit hungry-delete helm-projectile helm-flycheck helm-descbinds helm-ag gruvbox-theme grep+ git-gutter-fringe flatui-theme evil-leader dumb-jump dockerfile-mode docker dashboard color-theme-solarized base16-theme ag ack))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-dir-link-face ((t (:foreground "#FB4934"))))
 '(neo-file-link-face ((t (:foreground "#FAF4C1")))))
