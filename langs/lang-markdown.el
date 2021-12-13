;;; -*- lexical-binding: t -*-

(straight-use-package 'markdown-mode)

(autoloa #'markdown-mode "markdown-mode")
(with-eval-after-load "markdown-mode"
  (dolist (h '(markdown-mode-hook))
    (add-hook h #'flyspell-mode)))

(provide 'lang-markdown)
