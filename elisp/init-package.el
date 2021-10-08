;;; -*- lexical-binding: t -*-
;;; ------------------ MIRRORS, REPO, PKGS  ------------------


;; Commentary:
;; Swithed to straight.el. This file exits solely for searching through melpa, see
;; https://github.crookster.org/switching-to-straight.el-from-emacs-26-builtin-package.el/#step-2-convertremove-packageel-stuff

;; This code somwhow cause Emacs to rebuild every pkgs when it next starts up. Do not require this feature file in any other init files for now
(require 'package)
(setq package-archives '(("gnu"   . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
                         ("melpa" .  "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")
                         ("org"   . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/org/")))


(provide 'init-package)
;;; init-package ends here
