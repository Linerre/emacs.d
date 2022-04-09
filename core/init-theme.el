;;; -*- lexical-binding: t -*-

(sup 'kaolin-themes)
;; (emerge
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
      ;; (menu-bar-mode -1)
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


(provide 'init-theme)

;;; theme ends here
