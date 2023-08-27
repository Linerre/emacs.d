;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Font
(defvar +chinese-font-family "LXGW WenKai")

;; For Monospace use 16
(add-to-list 'default-frame-alist '(font . "Px437 IBM VGA 8x16-16"))
(set-fontset-font t 'han (font-spec :family +chinese-font-family))
(set-face-attribute 'variable-pitch nil :family "Sans" :font "Liberation Sans-16")
(set-face-attribute 'fixed-pitch nil :font "Px437 IBM VGA 8x16-16")

;; Theme
(defvar my-themes
  '(
    ;; alabaster
    chacha
    carbon
    console
    ft))

(defun +toggle-themes ()
  "Load the new-theme and disable the current one."
  (interactive)
  (setq my-themes (append (cdr my-themes) (list (car my-themes))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme (car my-themes) t))

(global-set-key (kbd "C-c m") #'+toggle-themes)


(blink-cursor-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(if (display-graphic-p)
    (load-theme 'chacha t nil)
    (load-theme 'console t nil))

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


(provide 'init-theme)

;;; init-theme.el ends here
