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
;; Projects root
(defconst *is-win* (string-equal system-type "windows-nt"))
(defconst *is-linux* (string-equal system-type "gnu/linux"))
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

;;; envs
(setenv "LSP_USE_PLISTS" "true")

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(setq-default mode-line-format nil)

;;; disable modes early
(blink-cursor-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(window-divider-mode 0)
(menu-bar-mode -1)

(require 'init-straight)

(provide 'early-init)

;;; early-init.el ends here
