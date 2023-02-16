;;; web-langs: html/css/javascript -*- lexical-binding: t -*-
;;; Author: Errelin
;;; Last Change:


(dolist (hook
         '(mhtml-mode-hook
           json-mode-hook
           svelte-mode-hook))
  (add-hook hook #'indent-guide-mode)
  (add-hook hook #'yas-minor-mode)
  (add-hook hook #'emmet-mode))

(with-eval-after-load "css-mode"
  (setq css-indent-offset 2)
  ;; (define-key company-active-map (kbd "SPC") #'company-complete-selection)
  )

(with-eval-after-load "js"
  (setq-default js-indent-level 2))

(with-eval-after-load "json-mode"
  (add-hook 'json-mode-hook #'flycheck-mode))
;; svelte
(autoload #'svelte-mode "svelte-mode" nil t)
;; (with-eval-after-load "svelte-mode")

(provide 'lang-web)
;;; lang-web.el ends here
