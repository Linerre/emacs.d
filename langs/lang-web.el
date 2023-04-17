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

(autoload #'svelte-mode "svelte-mode" nil t)
;; (with-eval-after-load "svelte-mode")

(with-eval-after-load "typescript-mode"
  (setq typescript-indent-level 2)
  (define-key typescript-mode-map (kbd "C-c C-l") #'lsp)
  (define-key typescript-mode-map (kbd "C-c C-a") #'lsp-execute-code-action))

(provide 'lang-web)
;;; lang-web.el ends here
