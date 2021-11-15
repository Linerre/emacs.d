;; -*- lexical-binding: t -*-

(straight-use-package 'highlight-defined)

(add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)

(provide 'lang-lisp)
