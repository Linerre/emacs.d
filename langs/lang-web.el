;;; web-langs: html/css/javascript -*- lexical-binding: t -*-
;;; Commentary
;;; Code

(dolist (hook
         '(mhtml-mode-hook
           json-mode-hook
           svelte-mode-hook
           tsx-ts-mode-hook
           typescript-ts-mode-hook))
  (add-hook hook #'yas-minor-mode)
  (add-hook hook #'emmet-mode))

(with-eval-after-load "css-mode"
  (setq css-indent-offset 2))

(setq-default js-indent-level 2)

(with-eval-after-load "json-mode"
  (add-hook 'json-mode-hook #'flycheck-mode))

(autoload #'svelte-mode "svelte-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . jtsx-tsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . jtsx-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(add-hook 'jtsx-jsx-mode #'hs-minor-mode)
(add-hook 'jtsx-tsx-mode #'hs-minor-mode)

(with-eval-after-load "jtsx"
  (define-key jtsx-tsx-mode-map (kbd "C-c C-l") #'lsp)
  (define-key jtsx-tsx-mode-map (kbd "C-c j o") #'jtsx-jump-jsx-opening-tag)
  (define-key jtsx-tsx-mode-map (kbd "C-c j c") #'jtsx-jump-jsx-closing-tag))

(with-eval-after-load "typescript-ts-mode"
  (add-hook 'typescript-ts-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-l") #'lsp)
              (local-set-key (kbd "C-c C-a") #'lsp-execute-code-action))))

(provide 'lang-web)
;;; lang-web.el ends here
