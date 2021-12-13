;;; -*- lexical-binding: t -*-
(straight-use-package
 '(go-mode.el :type git :host github :repo "dominikh/go-mode.el"))

(autoload #'go-mode "go-mode" nil t)
(provide 'lang-go)
