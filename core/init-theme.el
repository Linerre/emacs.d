;;; -*- lexical-binding: t -*-

;; (sup 'kaolin-themes)
;; (sup
;;  '(zenburn-emacs :type git :host github :repo "bbatsov/zenburn-emacs"))
(sup
 '(modus-themes :type git :host gitlab :repo "protesilaos/modus-themes"))

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
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (require 'modus-themes)
      (setq modus-themes-italic-constructs t
            modus-themes-bold-constructs nil
            modus-themes-region '(bg-only no-extend))
      (load-theme 'modus-operandi t))
  (progn
    (menu-bar-mode -1)                  ; almost useless in TUI
    (require 'modus-themes)
    (setq modus-themes-italic-constructs t
          modus-themes-bold-constructs nil
          modus-themes-region '(bg-only no-extend))
    (load-theme 'modus-vivendi t)
    ;; (require 'kaolin-themes)
    ;; (load-theme 'kaolin-light t)
    ;; (require 'kaolin-light-tweak-theme)
    ;; (load-theme 'kaolin-light-tweak t)
    ))

;; tree sidebar is useful when viewing a project
(sup 'dired-sidebar)
(setq dired-sidebar-theme 'ascii
      dired-sidebar-width 30
      dired-sidebar-use-custome-font t
      dired-sidebar-face '(:weight bold)

      ;; I don't like the default sidebar modeline format
      ;; It displays the current dir path, duplicated by the
      ;; buffer beginning line.
      ;; I want the git branch info instead but there's no
      ;; easy way to do that.
      ;; 10sr/git-ps1-mode-el seems to be a solution, though.
      ;; dired-sidebar-mode-line-format
      ;; '("%e" mode-line-front-space
      ;;   mode-line-mule-info
      ;;   (vc-mode vc-mode)
      ;;   " "  mode-line-end-spaces)
      )

;; To be decided yet
(sup
 '(git-ps1-mode-el
   :type git :host github :repo "10sr/git-ps1-mode-el"))

(autoload
  #'dired-sidebar-toggle-sidebar "dired-sidebar" nil t)
(global-set-key (kbd "<f8>") #'dired-sidebar-toggle-sidebar)
(with-eval-after-load "dired-sidebar"
  (add-hook 'dired-sidebar-mode-hook 'hl-line-mode))

(provide 'init-theme)

;;; theme ends here
