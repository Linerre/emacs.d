;; -*- lexical-binding: t -*-
;; ----------- LISP ------------

(straight-use-package 'highlight-defined)

(add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)

(provide 'init-lisp)
