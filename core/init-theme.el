;;; -*- lexical-binding: t -*-

(straight-use-package 'kaolin-themes)
<<<<<<< HEAD
(straight-use-package 'emacs-neotree)
=======
>>>>>>> dedbe5d (found back my lost sidebar!)
(straight-use-package 'dired-sidebar)
(straight-use-package
 '(zenburn-emacs :type git :host github :repo "bbatsov/zenburn-emacs"))


(blink-cursor-mode -1)

;; THEME-CHANGE
(defun +change-current-theme (new-theme)
  "Load the new-theme and disable the current theme."
  (interactive "SChange current theme to: ")
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme new-theme t))

(global-set-key (kbd "C-c m") #'+change-current-theme)

(if (display-graphic-p)
    (progn
      (require 'storybook-theme)
      (load-theme 'storybook t))
  (progn
    (require 'kaolin-themes)
    (load-theme 'kaolin-light t)
    (require 'kaolin-light-tweak-theme)
    (load-theme 'kaolin-light-tweak t)))

<<<<<<< HEAD
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

=======
>>>>>>> 0007395 (use LXGW Wenkai for Chinese characters)
;; tree
(autoload
  #'dired-sidebar-toggle-sidebar "dired-sidebar" nil t)
<<<<<<< HEAD
(global-set-key (kbd "C-v") #'dired-sidebar-toggle-sidebar)
=======
(global-set-key (kbd "C-c f") #'dired-sidebar-toggle-sidebar)
>>>>>>> dedbe5d (found back my lost sidebar!)

(with-eval-after-load "dired-sidebar"
  (add-hook 'dired-sidebar-mode-hook 'hl-line-mode)
  (setq dired-sidebar-theme 'ascii
        dired-sidebar-width 30
        dired-sidebar-use-custome-font t
        dired-sidebar-face '(:weight bold)))

(provide 'init-theme)

;;; theme ends here
