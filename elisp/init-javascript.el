;;; init-javascript.el  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(straight-use-package 'js2-mode)
(straight-use-package 'vue-mode)
(straight-use-package 'json-mode)

(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js-mode))

(with-eval-after-load "js-mode"
  (setq-default js-indent-level 2))


(provide 'init-javascript)
