;;; web-langs: html/css/javascript -*- lexical-binding: t -*-
;;; Author: Errelin
;;; Last Change:

(straight-use-package 'emmet-mode)
(straight-use-package 'jinja2-mode)
(straight-use-package 'js2-mode)
(straight-use-package
 '(vue-mode :type git :host github :repo "AdamNiederer/vue-mode"))
(straight-use-package
 '(svelte-mode :type git :host github :repo "leafOfTree/svelte-mode"))
(straight-use-package 'json-mode)

;; jinja2-mode
;; (autoload #'jinja2-mode "jinja2-mode")
(add-hook 'jinja2-mode-hook 'yas-minor-mode)
(add-hook 'jinja2-mode-hook 'visual-line-mode)

;; vue-mode is a major mode while emmet-mode is a minor one
;; use vue-mode for *.vue files
(add-hook 'mhtml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

(with-eval-after-load "css-mode"
  (setq css-indent-offset 2)
  (define-key company-active-map (kbd "SPC") #'company-complete-selection))

(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js-mode))

(with-eval-after-load "js-mode"
  (setq-default js-indent-level 2))

;; vue
;; (autoload #'vue-mode "vue-mode" nil t)
(with-eval-after-load "vue-mode"
  (add-hook 'vue-mode-hook 'emmet-mode)
  (add-hook 'vue-mode-hook 'hl-line-mode)
  (add-hook 'vue-mode-hook 'display-line-numbers-mode)
  (add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face nil))))
(provide 'lang-web)
;;; lang-web.el ends here
