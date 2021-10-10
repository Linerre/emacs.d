;;; -*- lexical-binding: t -*-
;; Commentary:
;; Code:

;; May need this
;; (setq package-check-signature nil)

(require 'package)
(setq package-archives '(("gnu"   . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
                         ("melpa" .  "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")
                         ("org"   . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/org/")))

;; Initialize package.el unless this already done
(unless (bound-and-true-p package--initialized)
  (package-initialize))
;;(when (memq window-system '(pc ns x))
;;  (exec-path-from-shell-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(defvar packages-list
  '(company
    eglot
    emmet-mode
    exec-path-from-shell
    flycheck

    highlight-defined

    jinja2-mode

    kaolin-themes
    magit
    markdown-mode
    meow
    org-roam
    ob-restclient
    selectrum
    smartparens
    yasnippet
    which-key))

(dolist (p packages-list)
  (unless (package-installed-p p)
    (package-install p)))


(provide 'init-packages)
;;; init-package ends here
