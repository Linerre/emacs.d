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
  (setq css-indent-offset 4))

(setq-default js-indent-level 4)

(with-eval-after-load "json-mode"
  (add-hook 'json-mode-hook #'flycheck-mode))

(autoload #'svelte-mode "svelte-mode" nil t)

;;; js, ts, jsx and tsx
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . jtsx-tsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jtsx-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.[mc]js\\'" . js-ts-mode))

(add-hook 'jtsx-jsx-mode-hook #'hs-minor-mode)
(add-hook 'jtsx-tsx-mode-hook #'hs-minor-mode)

(with-eval-after-load 'jtsx
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

;;; Typescript
;;; Hopefully Emacs 30.* would fix this by setting a saner value for this variable
(setq-default typescript-ts-mode-indent-offset 4)
(defun +typescript-special-puncs ()
  "Hightlight `!' and `?' in a different color"
  (setq treesit-font-lock-settings
        (append
         treesit-font-lock-settings
         (treesit-font-lock-rules
          :language 'typescript
          :feature 'operator
          :override t
          '((unary_expression operator: "!" @font-lock-misc-punctuation-face)
            (binary_expression operator: "??" @font-lock-misc-punctuation-face))

          :language 'typescript
          :feature 'string
          :override t
          '((template_substitution ["${" "}"] @font-lock-string-face))

          :language 'typescript
          :feature 'constant
          :override t
          '([(true) (false) (null) (undefined)] @font-lock-preprocessor-face)

          ;; :language 'typescript
          ;; :feature 'keyword
          ;; :override t
          ;; `([(this) (super)] @font-lock-preprocessor-face)
          ;; '((member_expression object: (this) @font-lock-preprocessor-face)) ; this works too
          )))
  (treesit-font-lock-recompute-features
   ;; '(operator string keyword) ; enable and merging these three features
   treesit-font-lock-feature-list   ; refresh the entire list instead of merging
   '(escape-sequence)               ; disable
   'typescript))                    ; only do this for typescript
;; If I want to truly remove `bracket' and `property' from feature list,
;; I should operate on `treesit-font-lock-feature-list' directly


(add-hook 'typescript-ts-mode-hook #'+typescript-special-puncs)
(add-hook 'typescript-ts-mode-hook
          (lambda ()
            (let* ((ts-settings (alist-get 'typescript treesit-thing-settings))
                   (new-ts-settings (cons `(sexp (not "[](),[{}]"))
                                          (cdr ts-settings))))
              (setq-local treesit-thing-settings
                          `((typescript . ,new-ts-settings))))))

(with-eval-after-load "typescript-ts-mode"
  (add-hook 'typescript-ts-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-l") #'lsp)
              (local-set-key (kbd "C-c C-a") #'lsp-execute-code-action))))

(provide 'lang-web)
;;; lang-web.el ends here
