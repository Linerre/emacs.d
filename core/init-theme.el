;;; -*- lexical-binding: t -*-

(straight-use-package 'kaolin-themes)

(require 'kaolin-themes)
(load-theme 'kaolin-light t)

;; load the cutomized theme here
(require 'kaolin-light-tweak-theme)
(load-theme 'kaolin-light-tweak t)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (cond (*is-mac*
         (add-to-list 'default-frame-alist
                      '(font . "IBM Plex Mono-16")))
        (*is-linux*
         (add-to-list 'default-frame-alist
                      ;'(font . "Courier New-16:bold")))))
                      '(font . "IBM Plex Mono-16")))))
;; use terminal theme/font in TUI with minor fixes
(when (not (display-graphic-p))
  (menu-bar-mode -1))

(provide 'init-theme)

;;; theme ends here
