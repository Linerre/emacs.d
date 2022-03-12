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
(require 'init-smartparens)
(require 'init-linter)
(require 'init-git)
(require 'lang-lisp)
(require 'lang-clojure)
(require 'lang-nix)
(require 'lang-rust)
(require 'lang-go)
(require 'lang-python)
(require 'lang-shell)
(require 'lang-sql)
(require 'lang-web)
(require 'lang-markdown)
(require 'lang-latex)
(require 'lang-org)

;;; init.el ends here
