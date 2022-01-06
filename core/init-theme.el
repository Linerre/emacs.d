;;; -*- lexical-binding: t -*-

(straight-use-package 'kaolin-themes)
(straight-use-package 'dired-sidebar)
(straight-use-package
 '(zenburn-emacs :type git :host github :repo "bbatsov/zenburn-emacs"))
(straight-use-package
   '(modus-themes :type git :host gitlab :repo "protesilaos/modus-themes"))

(blink-cursor-mode -1)

;; THEME-CHANGE
(defun +change-current-theme (new-theme)
  "Load the new-theme and disable the current theme."
  (interactive "SChange current theme to: ")
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme new-theme t))

(global-set-key (kbd "C-c m") #'+change-current-theme)

;; Use modus-operandi (light) on macOS with Emacs 27.2 GUI
(if (or (display-graphic-p) (version< emacs-version "28.0"))
    (progn
      (require 'modus-themes)
      (setq modus-themes-italic-constructs t
            modus-themes-bold-constructs nil
            modus-themes-region '(bg-only no-extend))
      (load-theme 'modus-operandi t))
  (progn
    (require 'kaolin-themes)
    (load-theme 'kaolin-light t)
    (require 'kaolin-light-tweak-theme)
    (load-theme 'kaolin-light-tweak t)))

;; font
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (setq default-frame-alist '((width . 120) (height . 48)))
  (cond (*is-mac*
         (add-to-list 'default-frame-alist
                      '(font . "Roboto Mono-16")))
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
(global-set-key (kbd "C-c f") #'dired-sidebar-toggle-sidebar)

(with-eval-after-load "dired-sidebar"
  (add-hook 'dired-sidebar-mode-hook 'hl-line-mode)
  (setq dired-sidebar-theme 'ascii
        dired-sidebar-width 30
        dired-sidebar-use-custome-font t
        dired-sidebar-face '(:weight bold)))

(provide 'init-theme)

;;; theme ends here
