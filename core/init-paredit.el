;;; -*- lexical-binding: t; -*-
;;; Commentary
;;; Code:

(sup 'paredit)

(autoload #'paredit-mode "paredit" nil t)

;; (with-eval-after-load "paredit"
;;   ;; hooks, keybinds, setq etc
;;   )

(dolist (h '(c-mode-hook
             c++-mode-hook
             css-mode-hook
             go-mode-hook
             jinja2-mode-hook
             js-mode-hook
             nix-mode-hook
             org-mode-hook
             python-mode-hook
             sql-mode-hook))
  (add-hook h #'electric-pair-mode))

(dolist (h '(c-mode-hook
             c++-mode-hook
             cider-repl-mode-hook
             css-mode-hook
             emacs-lisp-mode-hook
             lisp-mode-hook
             lisp-interaction-mode-hook))
  (add-hook h #'paredit-mode))

(provide 'init-paredit)
