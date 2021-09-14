;;; early-init.el -*- lexical-binding: t -*-

;;; Modularized initialization
;;; Author: Errelin
;;; Last Change:

;;; Modules stored in the "elisp" directory
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; see https://github.com/raxod502/straight.el#getting-started
(setq package-enable-at-startup nil)
(require 'init-straight)
(require 'init-package)
(require 'init-options)
