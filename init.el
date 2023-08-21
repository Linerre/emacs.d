;;; init.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;; Use `emacs --debug-init' when noticing any problems
;;; Code:

;;; Alias
(defalias 'sup 'straight-use-package "Another alias for straight-use-package.")

;;; Consts
(defconst *is-win* (string-equal system-type "windows-nt"))
(defconst *is-linux* (string-equal system-type "gnu/linux"))

;; Projects root
(defconst projects-root "~/projects/")
(setq custom-theme-directory "~/.emacs.d/themes/")

;;; On Linux, some packages will be installed by the system package manager:
;;; `emerge' on Gentoo and `pacman' on Arch.
(require 'init-packages)

;;; Load the modular configs
;; (require 'init-options)
(require 'init-utils)
(require 'init-modeline)
(require 'init-theme)
(require 'init-meow)
(require 'init-completion)
(require 'init-paredit)
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
(require 'init-mail)

;;; init.el ends here
