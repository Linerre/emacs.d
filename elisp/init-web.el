;;; -*- lexical-binding: t -*-
;;; -------------- WEB.EL -------------
;;; Author: Errelin
;;; Last Change:

(add-hook 'mhtml-mode-hook 'jinja2-mode)
(add-hook 'mhtml-mode-hook 'emmet-mode)
(add-hook 'jinja2-mode-hook 'emmet-mode)
(add-hook 'jinja2-mode-hook 'yas-minor-mode)
(add-hook 'jinja2-mode-hook 'visual-line-mode)
(add-hook 'css-mode 'emmet-mode)
(setq css-indent-offset 2)

(with-eval-after-load "company"
  (define-key company-active-map (kbd "SPC") #'company-complete-selection))

(provide 'init-web)
;;; init-web.el ends here
