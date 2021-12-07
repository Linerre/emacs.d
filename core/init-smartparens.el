;; -*- lexical-binding: t -*-
;; smart parens

(straight-use-package 'smartparens)
;; (setq sp-autowrap-region nil)
(autoload #'smartparens-mode "smartparens" nil t)

(with-eval-after-load "smartparens"
  (require 'smartparens-config)
  (define-key smartparens-mode-map (kbd "M-r") #'sp-raise-sexp)
  (define-key smartparens-mode-map (kbd "M-s") #'sp-unwrap-sexp)
  ;;(define-key smartparens-mode-map (kbd "M-[") #'sp-wrap-square)
  (define-key smartparens-mode-map (kbd "M-{") #'sp-wrap-curly)
  (define-key smartparens-mode-map (kbd "C-)") #'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-}") #'sp-forward-barf-sexp)
  ;; for lisp modes do not pair single quotes
  (sp-with-modes '(lisp-mode emacs-lisp-mode lisp-interaction-mode)
    (sp-local-pair "'" nil :actions nil)))

(dolist (h
         '(c-mode-hook
           go-mode-hook
           nix-mode-hook
           python-mode-hook
           css-mode-hook
           jinja2-mode-hook
           js-mode-hook))
  (add-hook h 'smartparens-mode))

(dolist (h '(lisp-mode-hook lisp-interaction-mode-hook emacs-lisp-mode-hook))
  (add-hook h 'smartparens-mode)
  (add-hook h 'smartparens-strict-mode))

(provide 'init-smartparens)
