;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'clojure-mode)
(straight-use-package 'cider)
(straight-use-package 'clj-refactor)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-clj-kondo)

;;; clojure-mode
(setq clojure-toplevel-inside-comment-form t)
(autoload #'clojure-mode "clojure-mode")

(with-eval-after-load "clojure-mode"
  ;; vars
  (setq-default fill-column 80)

  ;; hooks
  (add-hook #'clojure-mode 'cider-mode)
  (add-hook #'clojure-mode 'clj-refactor-mode)
  (add-hook 'clojure-mode-hook 'auto-fill-mode)
  (add-hook 'clojure-mode-hook 'display-fill-column-indicator-mode))

(with-eval-after-load "cider"
  ;; (subword-mode +1)
  (add-hook 'cider-mode-hook #'eldoc-mode))

(provide 'lang-clojure)

;; lang-clojure ends here
