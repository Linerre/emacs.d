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

;; By default Emacs (Makefile) adds this path to `load-path' and puts
;; a `subdirs.el' there to load all the sub-directories.  Yet on
;; Arch/Gentoo Emacs site-lisp points to `usr/share/emacs'.  Merely
;; adding the ``local'' version to `load-path' does not work!
;; Instead, treat it as any arbituary directory and this trick must be
;; used. For more see:
;; [1] https://lists.nongnu.org/archive/html/emacs-devel/2003-04/msg00315.html
;; [2] https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Search.html
;; [3] https://www.emacswiki.org/emacs/LoadPath
(let ((default-directory  "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;;; On Linux, some packages will be installed by the system package manager:
;;; `emerge' on Gentoo and `pacman' on Arch.
(require 'init-packages)

;;; Load the modular configs
(require 'init-options)
(require 'init-utils)
(require 'init-modeline)
(require 'init-theme)
(require 'init-meow)
(require 'init-completion)
(require 'init-paredit)
(require 'init-git)
(require 'lang-lisps)
(require 'lang-fp)
(require 'lang-rust)
(require 'lang-cpp)
(require 'lang-go)
(require 'lang-script)
(require 'lang-web)
(require 'lang-dsl)
;; (require 'lang-verilog)
;; (require 'init-mail)

;;; init.el ends here
