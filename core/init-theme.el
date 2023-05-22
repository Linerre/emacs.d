;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Font
(defvar +chinese-font-family "LXGW WenKai")

;; For Monospace use 16
(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-18"))
(set-fontset-font t 'han (font-spec :family +chinese-font-family))
(set-face-attribute 'variable-pitch nil :family "Sans Serif" :font "Crimson-24")
(set-face-attribute 'fixed-pitch nil :font "IBM Plex Mono-18")

;; Theme
(defvar my-themes
  '(alabaster
    carbon
    ft))

(defun +toggle-themes ()
  "Load the new-theme and disable the current one."
  (interactive)
  (setq my-themes (append (cdr my-themes) (list (car my-themes))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme (car my-themes) t))

(global-set-key (kbd "C-c m") #'+toggle-themes)

;; (setq modus-themes-italic-constructs t
;;       modus-themes-bold-constructs nil
;;       modus-themes-region '(bg-only no-extend))
;; (load-theme 'modus-operandi t)

(load-theme 'ft t nil)

(blink-cursor-mode -1)
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      ;; (load-theme 'alabaster t nil)
      )
  (progn
    ;; (load-theme 'alabaster t nil)
    (menu-bar-mode -1)))

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
