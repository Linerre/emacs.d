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
(add-hook #'clojure-mode 'cider-mode)
(add-hook #'clojure-mode 'clj-refactor-mode)

(provide 'lang-clojure)
