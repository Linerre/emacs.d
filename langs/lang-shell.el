;;; -*- lexical-binding: t; -*-

;;; Commentary:
;;; Shell-script-mode and shell-mode config

;;; Code:
(straight-use-package
 '(ebuild-mode :type git :host github :repo "emacsmirror/ebuild-mode"))
;; a child mode of shell-script-mode
(autoload #'ebuild-mode "ebuild-mode" nil t)


;; shell-script-mode
(add-to-list 'auto-mode-alist '("bash_profile\\'\\|[bz]a?shrc\\'" . sh-mode))

;; sh-script.el
(with-eval-after-load "sh-script"
  (add-hook 'sh-mode-hook 'electric-pair-mode))

;; vars can be set:
;; sh-basic-offset -- defaults to 4
;; sh-indentation -- defaults to 4 -- how many spaces to put in a deeper level
;; sh-indent-for-case-label
;; sh-indent-for-case-alt

(provide 'lang-shell)
