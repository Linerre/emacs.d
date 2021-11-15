;;; -*- lexical-binding: t -*-

(straight-use-package 'kaolin-themes)
(straight-use-package 'emacs-neotree)
(straight-use-package 'dired-sidebar)

(require 'kaolin-themes)
(load-theme 'kaolin-light t)

;; load the cutomized theme here
(require 'kaolin-light-tweak-theme)
(load-theme 'kaolin-light-tweak t)

;; font
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (setq default-frame-alist '((width . 169) (height . 48)))
  (cond (*is-mac*
         (add-to-list 'default-frame-alist
<<<<<<< HEAD
                      '(font . "Inconsolata-16")))
=======
                      '(font . "IBM Plex Mono-16")))
>>>>>>> 432bc88 (clean working tree)
        (*is-linux*
         (add-to-list 'default-frame-alist
                      ;'(font . "Courier New-16:bold")))))
                      '(font . "IBM Plex Mono-16")))))
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
        dired-sidebar-width 30
        dired-sidebar-use-custome-font t
        dired-sidebar-face '(:weight bold)))

(provide 'init-theme)

;;; theme ends here
