;;; -*- lexical-binding: t -*-

(defvar +chinese-font-family "LXGW WenKai")

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (cond (*is-mac*
         (add-to-list 'default-frame-alist
                      '(font . "IBM Plex Mono-18"))
         ;; use LXGW for chinese characters
         (set-fontset-font t 'han (font-spec :family +chinese-font-family)))

        (*is-linux*
         (add-to-list 'default-frame-alist
                      ;'(font . "Courier New-16:bold")))))
                      '(font . "IBM Plex Mono-16")))))
(when (not (display-graphic-p))
  (menu-bar-mode -1)
  (when *is-mac*
    (add-to-list 'default-frame-alist
                 '(font . "IBM Plex Mono-16"))))



(provide 'init-font)
