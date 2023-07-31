;;; -*- lexical-binding: t -*-

(defvar +chinese-font-family "LXGW WenKai")

;; GUI
(when (display-graphic-p)
  (when *is-linux*
    (add-to-list 'default-frame-alist '(font . "Roboto Mono")))

  ;; use LXGW for chinese characters
  (set-fontset-font t 'han (font-spec :family +chinese-font-family)))

(provide 'init-font)

;;; init-font.el ends here
