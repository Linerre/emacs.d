;;; -*- lexical-binding: t -*-

(straight-use-package 'kaolin-themes)
(straight-use-package 'emacs-neotree)
(straight-use-package 'dired-sidebar)

(require 'kaolin-themes)
(load-theme 'kaolin-light t)

;; load the cutomized theme here
(require 'roam-research-default-theme)
(load-theme 'roam-research-default t)

;; font
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (cond (*is-mac*
         (add-to-list 'default-frame-alist
                      '(font . "Sarasa Mono SC-16")))
        (*is-linux*
         (add-to-list 'default-frame-alist
                      ;'(font . "Courier New-16:bold")))))
                      '(font . "Liberation Mono-16")))))
;; use terminal theme/font in TUI with minor fixes
(when (not (display-graphic-p))
  (menu-bar-mode -1))

;; tree
(autoload
  #'dired-sidebar-toggle-sidebar "dired-sidebar" nil t)
(global-set-key (kbd "C-v") #'dired-sidebar-toggle-sidebar)

(with-eval-after-load "dired-sidebar"
  (add-hook 'dired-sidebar-mode-hook 'hl-line-mode)
  (setq dired-sidebar-theme 'ascii
        dired-sidebar-use-custome-font t
        dired-sidebar-face '(:weight bold)))

(provide 'init-theme)

;;; theme ends here
