;;; -*- lexical-binding: t; -*-

;;; Commentary:
;;; Packages installed by straight
;;; Code:

(sup 'auctex)
(sup 'auctex-lua)
(sup 'auctex-latexmk)
(sup 'cape)
(sup 'cider)
(sup 'clj-refactor)
(sup 'company)
(sup 'company-auctex)
(sup 'direnv)
(sup '(go-mode.el :type git :host github :repo "dominikh/go-mode.el"))
(sup 'geiser-chicken)
(sup 'geiser-guile)
(sup 'geiser-mit)
(sup 'json-mode)
(sup 'lsp-mode)
(sup 'flycheck)
(sup 'flycheck-clj-kondo)
(sup 'flycheck-joker)
(sup 'flycheck-pos-tip)
(sup 'haskell-mode)
(sup 'flymake-hlint)
(sup 'hindent)
(sup 'magit)
(sup 'markdown-mode)
(sup 'nix-mode)
(sup 'orderless)
;; coq-mode recognizes *.v files which conflict with verilog-mode, to avoid it:
;; (alist-get "\\.v\\'" auto-mode-alist nil t) ; t means to remove the key (pattern)
;; (add-to-list 'auto-mode-alist '("\\.coq\\'" . coq-mode))
;; that is, let coq-mode treat *.coq files as its majore filetype.
;; For now, I'm working on verilog only.
;; (sup 'proof-general)
(sup 'paredit)
(sup 'rg)
(sup 'cargo)
(sup '(sqlup-mode :type git :host github :repo "Trevoke/sqlup-mode.el"))
(sup '(svelte-mode :type git :host github :repo "leafOfTree/svelte-mode"))
(sup 'yasnippet)
(sup 'emmet-mode)
(sup 'clojure-mode)
(sup 'rust-mode)
(sup '(verilog-ts-mode :type git :host github :repo "gmlarumbe/verilog-ts-mode"))
(sup '(fpga :type git :host github :repo "gmlarumbe/fpga"))
(sup 'vundo)

;; To drop
;; (sup '(emacs-sql-indent :type git :host github :repo "alex-hhh/emacs-sql-indent"))

;;; Managed by pacman (Arch) or portage (Gentoo)
;; (sup '(meow :type git :host github :repo "DogLooksGood/meow")) installed via pacman/portage
;; (sup 'ebuild-mode)
(sup 'highlight-defined)

;;; Dropped
;; (sup 'cdlatex)
;; (sup 'cargo-mode)
;; (sup 'corfu)
;; (sup 'consult)
;; (sup 'embark)
;; (sup 'selectrum)
;; (sup 'kaolin-themes)
;; (sup '(modus-themes :type git :host gitlab :repo "protesilaos/modus-themes"))
;; (sup '(cmt :type git :host gitlab :repo "protesilaos/mct"))
;; (sup
;;  '(zenburn-emacs :type git :host github :repo "bbatsov/zenburn-emacs"))
;; (sup
;;  '(ford-this.el :type git :host github :repo "magnars/fold-this.el"))
;; (sup
;;  '(corfu-terminal
;;    :type git
;;    :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
;; (sup 'web-mode)
;; (sup 'marginalia)
;; (sup 'vertico)

(provide 'init-packages)

;;; init-packages.el ends here
