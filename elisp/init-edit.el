;;; settings for daily editting: keymaps and more
;;; Author: Errelin
;;; Last Change:

(straight-use-package 'which-key)
(which-key-mode)

;;; kill buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; open init.el
(defun open-init-file()
  (interactive)
  (find-file "~/.config/emacs/init.el"))

(global-set-key (kbd "<f2>") 'open-init-file)

;;; open org bank
(defun open-org-dir()
  (interactive)
  (find-file "~/projects/gkroam/org"))
(global-set-key (kbd "<f4>") 'open-org-dir)

;;; open posts
(defun open-longreads-dir()
  (interactive)
  (find-file "~/projects/posts/wechat"))
(global-set-key (kbd "<f5>") 'open-longreads-dir)

;;; open recent files
(require 'recentf)
(recentf-mode 1)
(setq recnetf-max-menu-item 10)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; other modules related to editting experience
(require 'init-windows)

;; it's good but when I switch between desktops it inserts
;; parens weirdly ... ONLY appears in TUI
;; due to "scroll alternate screen" option in terminal.app
(if (display-graphic-p)
    (require 'init-smartparens))

(provide 'init-edit)
;;; keymaps ends here