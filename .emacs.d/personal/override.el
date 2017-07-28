(message "Overriding...")

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
(setq org-journal-dir "~/Dropbox (Personal)/.org/journal/"
      org-journal-file-format "%Y%m%d.org")

(require 'org-bullets)
(setq org-bullets-bullet-list '("•"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'dashboard)
(setq dashboard-items '((recents  . 10) (bookmarks . 10) (projects . 10)))
(dashboard-setup-startup-hook)
