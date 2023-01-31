;;; -*- lexical-binding: t; -*-
;;; early-init.el
;;; Commentary:
;;; Borrowed from Purcell's config
;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "langs" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-straight)
(require 'init-options)
(provide 'early-init)

;;; early-init.el ends here
