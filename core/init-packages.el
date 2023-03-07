;;; -*- lexical-binding: t; -*-

;;; Commentary:
;;; Packages to be installed by straight
;;; Installed
(sup 'auctex)
(sup 'auctex-lua)
(sup 'auctex-latexmk)
(sup 'cape)
(sup 'cdlatex)
(sup 'cider)
(sup 'clojure-mode)
(sup 'clj-refactor)
(sup 'company)
(sup 'company-auctex)
(sup 'consult)
(sup 'corfu)
(sup '(cmt :type git :host gitlab :repo "protesilaos/mct"))
(sup 'direnv)
(sup '(emacs-sql-indent :type git :host github :repo "alex-hhh/emacs-sql-indent"))
(sup '(go-mode.el :type git :host github :repo "dominikh/go-mode.el"))
(sup '(indent-guide :type git :host github :repo "zk-phi/indent-guide"))
(sup 'json-mode)
(sup 'lsp-mode)
(sup 'flycheck)
(sup 'flycheck-clj-kondo)
(sup 'flycheck-joker)
(sup 'flycheck-pos-tip)
(sup 'magit)
(sup 'marginalia)
(sup 'markdown-mode)
(sup 'nix-mode)
(sup 'orderless)
(sup 'paredit)
(sup 'rg)
(sup 'rustic)
(sup '(sqlup-mode :type git :host github :repo "Trevoke/sqlup-mode.el"))
(sup '(svelte-mode :type git :host github :repo "leafOfTree/svelte-mode"))
(sup 'vertico)
(sup 'which-key)
(sup 'yasnippet)
(sup 'emmet-mode)

(require 'vundo)
;;; Managed by pacman (Arch) or portage (Gentoo)
;; (sup '(meow :type git :host github :repo "DogLooksGood/meow")) installed via pacman/portage
;; (sup 'vundo)
;; (sup 'ebuild-mode)
;; (sup 'highlight-defined)

;;; Dropped
;; (sup 'cargo-mode)
;; (sup 'rust-mode)
;; (sup 'selectrum)
;; (sup 'kaolin-themes)
;; (sup '(modus-themes :type git :host gitlab :repo "protesilaos/modus-themes"))
;; (sup
;;  '(zenburn-emacs :type git :host github :repo "bbatsov/zenburn-emacs"))
;; (sup
;;  '(ford-this.el :type git :host github :repo "magnars/fold-this.el"))
;; (sup
;;  '(corfu-terminal
;;    :type git
;;    :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
;; (sup 'web-mode)

(provide 'init-packages)
