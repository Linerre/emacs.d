;;; web-langs: html/css/javascript -*- lexical-binding: t -*-
;;; Author: Errelin
;;; Last Change:

(straight-use-package 'emmet-mode)
(straight-use-package 'jinja2-mode)
(straight-use-package 'js2-mode)
(straight-use-package 'web-mode)
(straight-use-package
 '(indent-guide :type git :host github :repo "zk-phi/indent-guide"))
(straight-use-package
 '(vue-mode :type git :host github :repo "AdamNiederer/vue-mode"))
(straight-use-package
 '(svelte-mode :type git :host github :repo "leafOfTree/svelte-mode"))
(straight-use-package 'json-mode)

(autoload #'web-mode "web-mode" nil t)

(dolist (hook
         '(jinja2-mode-hook
           mhtml-mode-hook
           js-mode-hook
           json-mode-hook
           svelte-mode-hook
           vue-mode-hook
           web-mode-hook))
  (add-hook hook #'indent-guide-mode)
  (add-hook hook #'smartparens-mode)
  (add-hook hook #'yas-minor-mode)
  (add-hook hook #'emmet-mode))

(with-eval-after-load "web-mode"
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(with-eval-after-load "css-mode"
  (setq css-indent-offset 2)
  (add-hook 'css-mode-hook 'emmet-mode)
  (define-key company-active-map (kbd "SPC") #'company-complete-selection))

(autoload #'json-mode "json-mode" nil t)
(with-eval-after-load "js"
  (setq-default js-indent-level 2))

;; vue
(autoload #'vue-mode "vue-mode" nil t)
;; vue-mode is a major mode while emmet-mode is a minor one
;; use vue-mode for *.vue files
(with-eval-after-load "vue-mode"
  (add-hook 'vue-mode-hook 'hl-line-mode)
  (add-hook 'vue-mode-hook 'display-line-numbers-mode)
  (add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face nil))))

;; svelte
(autoload #'svelte-mode "svelte-mode" nil t)
;; (with-eval-after-load "svelte-mode")

(provide 'lang-web)
;;; lang-web.el ends here
