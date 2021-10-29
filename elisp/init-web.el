;;; -*- lexical-binding: t -*-
;;; Author: Errelin
;;; Last Change:

(straight-use-package 'emmet-mode)
(straight-use-package 'jinja2-mode)

;; jinja2-mode
(add-hook 'jinja2-mode-hook 'yas-minor-mode)
(add-hook 'jinja2-mode-hook 'visual-line-mode)

;; vue-mode is a major mode while emmet-mode is a minor one
;; use vue-mode for *.vue files
;;(add-hook 'mhtml-mode-hook 'vue-mode)
(add-hook 'mhtml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(setq css-indent-offset 2)

(with-eval-after-load "css-mode"
  (define-key company-active-map (kbd "SPC") #'company-complete-selection))

(provide 'init-web)
;;; init-web.el ends here
