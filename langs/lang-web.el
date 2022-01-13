;;; web-langs: html/css/javascript -*- lexical-binding: t -*-
;;; Author: Errelin
;;; Last Change:

(straight-use-package 'emmet-mode)
(straight-use-package 'web-mode)
(straight-use-package
 '(indent-guide :type git :host github :repo "zk-phi/indent-guide"))

(straight-use-package
 '(svelte-mode :type git :host github :repo "leafOfTree/svelte-mode"))
(straight-use-package 'json-mode)

;; web-mode
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2)

(dolist (hook
         '(mhtml-mode-hook
           css-mode-hook
           json-mode-hook
           svelte-mode-hook
           web-mode-hook))
  (add-hook hook #'indent-guide-mode)
  (add-hook hook #'smartparens-mode)
  (add-hook hook #'yas-minor-mode)
  (add-hook hook #'emmet-mode))

(autoload #'web-mode "web-mode" nil t)
(with-eval-after-load "web-mode"
  (setq web-mode-enable-auto-closing t))

(with-eval-after-load "css-mode"
  (setq css-indent-offset 2)
  ;; (define-key company-active-map (kbd "SPC") #'company-complete-selection)
  )

(autoload #'json-mode "json-mode" nil t)
(with-eval-after-load "js"
  (setq-default js-indent-level 2))

;; svelte
(autoload #'svelte-mode "svelte-mode" nil t)
;; (with-eval-after-load "svelte-mode")

(provide 'lang-web)
;;; lang-web.el ends here
