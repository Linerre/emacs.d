;;; init-javascript.el  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js-mode))

(setq-default js-indent-level 2)

(provide 'init-javascript)
