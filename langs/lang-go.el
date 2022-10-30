;;; -*- lexical-binding: t -*-
(sup
 '(go-mode.el :type git :host github :repo "dominikh/go-mode.el"))

(autoload #'go-mode "go-mode" nil t)
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))

(provide 'lang-go)
;;; lang-go.el ends here
