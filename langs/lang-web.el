;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; For web developement langauges: HTML/CSS/JavaScript/TypeScript/Json
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

(setq-default js-indent-level 4)

(with-eval-after-load "json-mode"
  (add-hook 'json-mode-hook #'flycheck-mode))

(autoload #'svelte-mode "svelte-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . jtsx-tsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . jtsx-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.[mc]js\\'" . js-mode))

(add-hook 'jtsx-jsx-mode-hook #'hs-minor-mode)
(add-hook 'jtsx-tsx-mode-hook #'hs-minor-mode)

(with-eval-after-load "jtsx"
  (setq jtsx-enable-jsx-element-tags-auto-sync t)
  (setq lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))
  (add-hook 'jtsx-tsx-mode-hook #'flycheck-mode)
  (define-key jtsx-tsx-mode-map (kbd "C-c C-l") #'lsp)
  (define-key jtsx-tsx-mode-map (kbd "C-c j o") #'jtsx-jump-jsx-opening-tag)
  (define-key jtsx-tsx-mode-map (kbd "C-c j c") #'jtsx-jump-jsx-closing-tag)
  (define-key jtsx-tsx-mode-map (kbd "C-c j r") #'jtsx-rename-jsx-element)
  (define-key jtsx-tsx-mode-map (kbd "C-c <down>") #'jtsx-move-jsx-element-tag-forward)
  (define-key jtsx-tsx-mode-map (kbd "C-c <up>") #'jtsx-move-jsx-element-tag-backward)
  (define-key jtsx-tsx-mode-map (kbd "C-c C-<down>") #'jtsx-move-jsx-element-forward)
  (define-key jtsx-tsx-mode-map (kbd "C-c C-<up>") #'jtsx-move-jsx-element-backward)
  (define-key jtsx-tsx-mode-map (kbd "C-c C-S-<down>")
              #'jtsx-move-jsx-element-step-in-forward)
  (define-key jtsx-tsx-mode-map (kbd "C-c C-S-<up>")
              #'jtsx-move-jsx-element-step-in-backward)
  (define-key jtsx-tsx-mode-map (kbd "C-c j w") #'jtsx-wrap-in-jsx-element)
  (define-key jtsx-tsx-mode-map (kbd "C-c j u") #'jtsx-unwrap-jsx)
  (define-key jtsx-tsx-mode-map (kbd "C-c j d") #'jtsx-delete-jsx-node)
  (define-key jtsx-tsx-mode-map (kbd "C-c j t") #'jtsx-toggle-jsx-attributes-orientation)
  (define-key jtsx-tsx-mode-map (kbd "C-c j h") #'jtsx-rearrange-jsx-attributes-horizontally)
  (define-key jtsx-tsx-mode-map (kbd "C-c j v") #'jtsx-rearrange-jsx-attributes-vertically))

(with-eval-after-load "typescript-ts-mode"
  (add-hook 'typescript-ts-mode-hook #'flycheck-mode)
  (add-hook 'typescript-ts-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-l") #'lsp)
              (local-set-key (kbd "C-c C-a") #'lsp-execute-code-action))))

(provide 'lang-web)
;;; lang-web.el ends here
