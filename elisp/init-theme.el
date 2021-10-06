;;; -*- lexical-binding: t -*-
;;; ----------------------- THEME ----------------------


(straight-use-package 'kaolin-themes)
(load-theme 'kaolin-light t)

;; Disable tool bar, and scroll bar in GUI
;; Set font as well
;; Use display-graphic-p to detect gui(t) or tui(nil, text-only terminals, even running on DE)
;; Use window-system to detect OS type: nil->char-only terminals; w32->windows; ns->macOS (deprecated according to GNU Emacs Manual)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  ;;(load-theme 'kaolin-light t)
  (add-to-list 'default-frame-alist
               '(font . "Courier-18")))

;; use terminal theme/font in TUI with minor fixes
(when (not (display-graphic-p))

  (menu-bar-mode -1))

(provide 'init-theme)
;;; theme ends here
