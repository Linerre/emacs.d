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

;; only need this on macOS or Windows
(when (memq window-system '(pc ns))
  (exec-path-from-shell-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(defvar packages-list
  '(auctex
    auctex-lua
    auctex-latexmk
    cdlatex
    company
    cider
    clojure-mode
    clj-refactor

    eglot
    emmet-mode
    exec-path-from-shell
    flycheck
    flycheck-clj-kondo
    highlight-defined

    jinja2-mode
    js2-mode
    json-mode

    kaolin-themes
    magit
    markdown-mode
    meow
    nix-mode
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
