;;; web-langs: html/css/javascript -*- lexical-binding: t -*-
;;; Author: Errelin
;;; Last Change:


;; web-mode
;; (setq web-mode-markup-indent-offset 2
;;       web-mode-css-indent-offset 2
;;       web-mode-code-indent-offset 2)

(dolist (hook
         '(mhtml-mode-hook
           json-mode-hook
           svelte-mode-hook
           web-mode-hook))
  (add-hook hook #'indent-guide-mode)
  (add-hook hook #'yas-minor-mode)
  (add-hook hook #'emmet-mode))

;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.s?css\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))

;; (with-eval-after-load "web-mode"
;;   (setq web-mode-enable-auto-closing t))

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
