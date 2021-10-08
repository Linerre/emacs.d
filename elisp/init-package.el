;;; -*- lexical-binding: t -*-
;;; ------------------ MIRRORS, REPO, PKGS  ------------------
;;; ---------------------- PACKAGE.EL ------------------------

;; Commentary:
;; Swithed to straight.el. This file exits solely for searching through melpa, see
;; https://github.crookster.org/switching-to-straight.el-from-emacs-26-builtin-package.el/#step-2-convertremove-packageel-stuff
;;(require 'package)
;;(setq package-archives '(("gnu"   . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
;;                         ("melpa" . ;; "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")
;                         ("org"   . ;;"https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/org/")))

(straight-use-package
 '(exec-path-from-shell
   :type git :host github :repo "purcell/exec-path-from-shell"))
 (when (memq window-system '(pc ns x))
   (exec-path-from-shell-initialize))

(provide 'init-package)
;;; init-package ends here
