;;; web-langs: html/css/javascript -*- lexical-binding: t -*-
;;; Commentary
;;; Code

(dolist (hook
         '(mhtml-mode-hook
           json-mode-hook
           svelte-mode-hook))
  (add-hook hook #'yas-minor-mode)
  (add-hook hook #'emmet-mode))

(with-eval-after-load "css-mode"
  (setq css-indent-offset 2)
  )

(setq-default js-indent-level 2)

(with-eval-after-load "json-mode"
  (add-hook 'json-mode-hook #'flycheck-mode))

(autoload #'svelte-mode "svelte-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(with-eval-after-load "typescript-ts-mode"
  (add-hook 'typescript-ts-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-l") #'lsp)
              (local-set-key (kbd "C-c C-a") #'lsp-execute-code-action))))

(provide 'lang-web)
;;; lang-web.el ends here
