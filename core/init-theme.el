;;; -*- lexical-binding: t -*-

;;; Font
(defvar +chinese-font-family "LXGW WenKai")

(when (display-graphic-p)
  (when *is-mac*
    (setq default-frame-alist '((width . 120) (height . 48)))
    (add-to-list 'default-frame-alist '(font . "Menlo-18")))

  (when *is-linux*
    (add-to-list 'default-frame-alist '(font . "Inconsolata-16")))

  ;; use LXGW for chinese characters
  (set-fontset-font t 'han (font-spec :family +chinese-font-family)))

;; THEME-CHANGE
(defun +change-current-theme (new-theme)
  "Load the new-theme and disable the current theme."
  (interactive "SChange current theme to: ")
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme new-theme t))

(global-set-key (kbd "C-c m") #'+change-current-theme)


(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-region '(bg-only no-extend))
(load-theme 'modus-operandi t)

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1))
  (progn
    (menu-bar-mode -1)
    ;; (require 'kaolin-themes)
    ;; (load-theme 'kaolin-light t)
    ;; (require 'kaolin-light-tweak-theme)
    ;; (load-theme 'kaolin-light-tweak t))
    ))

;; tree sidebar is useful when viewing a project
(sup 'dired-sidebar)
(setq dired-sidebar-theme 'ascii
      dired-sidebar-width 30
      dired-sidebar-use-custome-font t
      dired-sidebar-face '(:weight bold))

(autoload
  #'dired-sidebar-toggle-sidebar "dired-sidebar" nil t)
(global-set-key (kbd "<f8>") #'dired-sidebar-toggle-sidebar)
(with-eval-after-load "dired-sidebar"
  (add-hook 'dired-sidebar-mode-hook 'hl-line-mode))

(blink-cursor-mode -1)

(provide 'init-theme)

;;; theme ends here
