;;; -*- lexical-binding: t; -*-

;;; Commentary:
;;; Shell-script-mode and shell-mode config

;;; Code:

;; shell-script-mode
(add-to-list 'auto-mode-alist '("\\.sh\\'\\|\\.?[bz]a?shrc" . sh-mode))

;; (with-eval-after-load "sh-script"
;;   (message "Shell script mode"))

(provide 'lang-shell)
