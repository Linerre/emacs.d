;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; clojure-mode

(setq clojure-toplevel-inside-comment-form t)
(autoload #'clojure-mode "clojure-mode")
(add-hook #'clojure-mode 'cider-mode)
(add-hook #'clojure-mode 'clj-refactor-mode)
