;;; -*- lexical-binding: t -*-
;;; settings for daily editting: keymaps and more

(straight-use-package 'which-key)

;;; kill buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; open init.el
(defun open-init-dir()
  (interactive)
  (find-file "~/.emacs.d"))

(global-set-key (kbd "<f2>") 'open-init-dir)

;;; open org bank
(defun open-org-dir()
  (interactive)
  (find-file "~/projects/org"))
(global-set-key (kbd "<f6>") 'open-org-dir)

;;; open posts
(defun open-longreads-dir()
  (interactive)
  (find-file "~/projects/posts/wechat"))
(global-set-key (kbd "C-c r") 'open-longreads-dir)

;; other modules related to editting experience
(require 'which-key)
(which-key-mode)

(provide 'init-edit)
;;; keymaps ends here
