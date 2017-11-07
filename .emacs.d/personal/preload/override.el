;; Theme
(disable-theme 'zenburn)
(setq prelude-theme nil)

(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

;; Window
(when window-system
  (toggle-scroll-bar -1)
  (setq frame-resize-pixelwise t)
  (set-face-attribute 'default nil :height 150))

;; Evil
(setq evil-want-C-u-scroll t)
