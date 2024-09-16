;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Fonts
(defvar +chinese-font-family "SourceHanSans")
(defvar bitmap-font "Px437 IBM VGA 8x16-16")
(defvar sans-font "Liberation Sans-16")
(defvar serif-font "Liberation Serif-16")
(defvar mono-font-liberation "Liberation Mono-13")
(defvar mono-dejavu "DejaVu Sans Mono-14")
(defvar mono-font-inconsolata "Inconsolata-14")
(defvar mono-font-consolas "Consolas-14")
;; (defvar mono-font "Monospace-13")

(add-to-list 'default-frame-alist `(font . ,mono-dejavu))
(set-fontset-font t 'han (font-spec :family +chinese-font-family))
(set-face-attribute 'variable-pitch nil :family "Sans" :font sans-font)
(set-face-attribute 'fixed-pitch nil :font mono-dejavu)

;; Themes
(defvar my-themes
  '(
    ;; alabaster
    chacha
    carbon
    console
    gruber-darker
    ft))

(defun +toggle-themes ()
  "Load the new-theme and disable the current one."
  (interactive)
  (setq my-themes (append (cdr my-themes) (list (car my-themes))))
  (mapc #'disable-theme custom-enabled-themes)
  (let ((theme (car my-themes)))
    (load-theme theme t nil)))

(global-set-key (kbd "C-c m") #'+toggle-themes)

(blink-cursor-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; (menu-bar-mode -1)

(if (display-graphic-p)
    (load-theme 'chacha t nil)
  (load-theme 'gruber-darker t nil))

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
