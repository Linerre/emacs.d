;;; init.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;; Modules are categorized into two groups:
;;; core/init-* are essentials for all types of editting in Emacs;
;;; langs/lang-* specify preferences for langs

;;; Code:
(require 'init-utils)
(require 'init-font)
(require 'init-theme)
(require 'init-meow)
(require 'init-completion)
(require 'init-mail)
(require 'init-paredit)
(require 'init-linter)
(require 'init-git)
(require 'lang-lisp)
(require 'lang-clojure)
(require 'lang-rust)
(require 'lang-cpp)
(require 'lang-go)
(require 'lang-python)
(require 'lang-shell)
(require 'lang-web)
(require 'lang-dsl)

;;; init.el ends here
