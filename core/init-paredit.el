;;; -*- lexical-binding: t; -*-
;;; Commentary
;;; Code:

(sup 'paredit)

(autoload #'paredit-mode "paredit" nil t)

(dolist (h '(c-mode-hook
             c++-mode-hook
             css-mode-hook
             go-mode-hook
             jinja2-mode-hook
             js-mode-hook
             lua-mode-hook
             nix-mode-hook
             org-mode-hook
             python-mode-hook
             sql-mode-hook))
  (add-hook h #'electric-pair-local-mode))

(dolist (h '(cider-repl-mode-hook
             clojure-mode-hook
             clojurescript-mode-hook
             emacs-lisp-mode-hook
             lisp-mode-hook
             lisp-interaction-mode-hook))
  (add-hook h #'paredit-mode))

(provide 'init-paredit)
