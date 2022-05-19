;;; -*- lexical-binding: t -*-

(defvar +chinese-font-family "LXGW WenKai")

;; GUI
(when (display-graphic-p)
  (when *is-mac*
    (setq default-frame-alist '((width . 120) (height . 48))))
  (add-to-list 'default-frame-alist '(font . "Roboto Mono-18"))
  ;; use LXGW for chinese characters
  (set-fontset-font t 'han (font-spec :family +chinese-font-family)))

(provide 'init-font)

;;; init-font.el ends here
