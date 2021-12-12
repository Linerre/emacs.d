;; -*- lexical-binding: t -*-

(straight-use-package 'highlight-defined)

;; elisp
(with-eval-after-load "elisp-mode"
  (add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode)
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode))

(provide 'lang-lisp)

;; lang-list ends here
