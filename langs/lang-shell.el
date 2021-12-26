;;; -*- lexical-binding: t; -*-

;;; Commentary:
;;; Shell-script-mode and shell-mode config

;;; Code:

;; shell-script-mode
(add-to-list 'auto-mode-alist '("bash_profile\\'\\|[bz]a?shrc\\'" . sh-mode))

;; (with-eval-after-load "sh-script"
;; vars can be set:
;; sh-basic-offset -- defaults to 4
;; sh-indentation -- defaults to 4 -- how many spaces to put in a deeper level
;; sh-indent-for-case-label
;; sh-indent-for-case-alt
;;   (message "Shell script mode"))

(provide 'lang-shell)
