;;; -*- lexical-binding: t -*-

(defvar +chinese-font-family "LXGW WenKai")

(when (display-graphic-p)
  (cond (*is-mac*
         (setq default-frame-alist '((width . 120) (height . 48)))
         (add-to-list 'default-frame-alist '(font . "Roboto Mono-18"))
         ;; use LXGW for chinese characters
         (set-fontset-font t 'han (font-spec :family +chinese-font-family)))

        (*is-linux*
         (add-to-list 'default-frame-alist
                      '(font . "IBM Plex Mono-16")))))

;; TUI
(when (not (display-graphic-p))
  (menu-bar-mode -1)
  (when *is-mac*
    (add-to-list 'default-frame-alist
                 ;; '(font . "IBM Plex Mono-16")
                 '(font . "Roboto Mono-18"))))

(provide 'init-font)

;;; init-font.el ends here
