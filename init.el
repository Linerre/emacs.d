;;;  -*- lexical-binding: t -*-

;;; Commentary:
;;; Modularized initialization
;;; Author: Errelin
;;; Last Change: Sat Aug 21 23:41:36 2021
;;; Modules stored in the "elisp" directory

;;; Code:
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-options)
(require 'init-utils)
(require 'init-packages)
(require 'init-theme)
(require 'init-modal-qwerty)
(require 'init-edit)
(require 'init-completion)
(require 'init-lsp)
(require 'init-git)
(require 'init-lisp)
(require 'init-python)
(require 'init-javascript)
(require 'init-web)
(require 'init-modeline)
(require 'init-org)
(require 'init-latex)
(require 'init-server)
