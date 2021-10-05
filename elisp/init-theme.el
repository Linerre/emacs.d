;;; ----------------------- THEME ----------------------
;;(require 'kaolin-themes)

(straight-use-package 'kaolin-themes)
(straight-use-package
 '(emacs-theme-gruvbox
   :type git
   :host github
   :repo "greduan/emacs-theme-gruvbox"))



;; Disable tool bar, and scroll bar in gui
;; Set font as well
;; Use display-graphic-p to detect gui(t) or tui(nil, text-only terminals, even running on DE)
;; Use window-system to detect OS type: nil->char-only terminals; w32->windows; ns->macOS (deprecated according to GNU Emacs Manual)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (load-theme 'kaolin-light t)
  (add-to-list 'default-frame-alist
               '(font . "Courier-18")))

(when (not (display-graphic-p))
  ;;(load-theme 'gruvbox-light-soft t)
  (menu-bar-mode -1))

(provide 'init-theme)
;;; theme ends here
