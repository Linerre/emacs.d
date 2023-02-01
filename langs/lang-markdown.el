;;; -*- lexical-binding: t -*-

(sup 'markdown-mode)

(autoload #'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(autoload #'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)

(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(with-eval-after-load "markdown-mode"
  (add-hook 'markdown-mode-hook #'flyspell-mode))

(provide 'lang-markdown)
