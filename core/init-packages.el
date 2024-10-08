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
(sup 'consult)
(sup 'embark)
(sup '(emacs-ccls :type git :host github :repo "emacs-lsp/emacs-ccls"))
(sup 'direnv)
(sup '(go-mode.el :type git :host github :repo "dominikh/go-mode.el"))
(sup 'geiser-chicken)
(sup 'geiser-guile)
(sup 'geiser-chez)
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
(sup 'magit-delta)
(sup 'markdown-mode)
(sup '(move-mode :type git :host github :repo "amnn/move-mode"))
(sup 'nix-mode)
(sup 'orderless)
;; coq-mode recognizes *.v files which conflict with verilog-mode, to avoid it:
;; (alist-get "\\.v\\'" auto-mode-alist nil t) ; t means to remove the key (pattern)
;; (add-to-list 'auto-mode-alist '("\\.coq\\'" . coq-mode))
;; that is, let coq-mode treat *.coq files as its majore filetype.
;; For now, I'm working on coq only.
(sup 'proof-general)
(sup 'company-coq)
(sup 'paredit)
(sup 'rg)
(sup 'cargo)
(sup '(sqlup-mode :type git :host github :repo "Trevoke/sqlup-mode.el"))
(sup '(svelte-mode :type git :host github :repo "leafOfTree/svelte-mode"))
(sup 'yasnippet)
(sup 'emmet-mode)
(sup 'clojure-mode)
(sup 'rust-mode)
;; (sup '(verilog-mode :type git :host github :repo "veripool/verilog-mode"))
;; (sup '(verilog-ts-mode :type git :host github :repo "gmlarumbe/verilog-ts-mode"))
;; (sup '(fpga :type git :host github :repo "gmlarumbe/fpga"))
(sup 'vundo)
(sup '(zk :type git :host github :repo "localauthor/zk"))
(sup '(jstx :type git :host github :repo "llemaitre19/jtsx"))

;;; Managed by pacman (Arch) or portage (Gentoo)
;; (sup '(meow :type git :host github :repo "DogLooksGood/meow")) installed via pacman/portage
;; (sup 'ebuild-mode)
(sup 'highlight-defined)
(sup 'vertico)

;;; Dropped
;; (sup 'cdlatex)
;; (sup 'cargo-mode)
;; (sup 'corfu)
;; (sup
;;  '(zenburn-emacs :type git :host github :repo "bbatsov/zenburn-emacs"))
;; (sup
;;  '(corfu-terminal
;;    :type git
;;    :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
;; (sup 'marginalia)

(provide 'init-packages)

;;; init-packages.el ends here
