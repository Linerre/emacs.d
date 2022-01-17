;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'clojure-mode)
(straight-use-package 'cider)
(straight-use-package 'clj-refactor)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-clj-kondo)
(straight-use-package 'flycheck-joker)
(straight-use-package 'paredit)

;;; clojure-mode
(setq clojure-toplevel-inside-comment-form t)
(autoload #'clojure-mode "clojure-mode" nil t)

(with-eval-after-load "clojure-mode"
  (add-hook 'clojure-mode-hook 'paredit-mode)
  ;; cider-jack-in will enable cider-mode so there's no need to hook it to
  ;; clojure-mode. But doing so will allow immediate access to cider-mode
  ;; after entering a clj buffer. REPL is not always necessary however.
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (define-key clojure-mode-map (kbd "C-c j") #'cider-jack-in)
  (define-key clojure-mode-map (kbd "C-c c") #'cider-connect))

;; cider-mode -- an Emacs minor mode
(with-eval-after-load "cider"
  (setq nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  ;; Use Emacs native keybindings in cider-repl
  (add-hook 'cider-repl-mode-hook (lambda () (meow-mode -1)))
  (define-key cider-repl-mode-map (kbd "C-c h") 'windmove-left)
  (define-key cider-repl-mode-map (kbd "C-c l") 'windmove-right)
  (define-key cider-repl-mode-map (kbd "C-c j") 'windmove-down)
  (define-key cider-repl-mode-map (kbd "C-c k") 'windmove-up))

(provide 'lang-clojure)
;;; lang-clojure.el ends here
