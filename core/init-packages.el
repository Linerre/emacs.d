;;; -*- lexical-binding: t; -*-

;;; Commentary:
;;; Packages installed by straight
;;; Code:

(sup 'auctex)
(sup 'auctex-lua)
(sup 'auctex-latexmk)
(sup 'cape)
(sup 'cider)
(sup 'citre)
(sup 'corfu)
(sup 'company)
(sup 'company-auctex)
(sup 'consult)
(sup 'embark)
(sup '(emacs-ccls :host github :repo "emacs-lsp/emacs-ccls"))
(sup '(eglot-booster :host github :repo "jdtsmith/eglot-booster"))
(sup '(eat
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))
(sup '(go-mode.el :type git :host github :repo "dominikh/go-mode.el"))
(sup 'geiser-chicken)
(sup 'geiser-guile)
(sup 'geiser-chez)
(sup 'geiser-mit)
(sup '(gptel :type git :host github :repo "karthink/gptel"))
(sup 'json-mode)
(sup 'just-mode)
(sup '(justl.el :type git :host github :repo "psibi/justl.el"))
(sup 'lsp-mode)
(sup 'flycheck)
(sup 'flycheck-clj-kondo)
(sup 'flycheck-joker)
(sup 'flycheck-pos-tip)
(sup 'haskell-mode)
(sup 'flymake-hlint)
(sup 'forge)
(sup 'hindent)
(sup 'magit)
(sup 'markdown-mode)
(sup '(meow :type git :host github :repo "meow-edit/meow"))
(sup '(move-mode :host github :repo "Linerre/move-mode"))
(sup 'nix-mode)
(sup 'orderless)
;; coq-mode recognizes *.v files which conflict with verilog-mode, to avoid it:
;; (alist-get "\\.v\\'" auto-mode-alist nil t) ; t means to remove the key (pattern)
;; (add-to-list 'auto-mode-alist '("\\.coq\\'" . coq-mode))
;; that is, let coq-mode treat *.coq files as its major filetype.
;; For now, I'm working on coq only.
(sup 'proof-general)
(sup 'company-coq)
(sup 'paredit)
(sup 'pass)
(sup 'rg)
(sup '(sqlup-mode :type git :host github :repo "Trevoke/sqlup-mode.el"))
(sup '(svelte-mode :type git :host github :repo "leafOfTree/svelte-mode"))
(sup 'tide)
(sup 'yasnippet)
(sup 'emmet-mode)
(sup 'clojure-mode)
(sup 'cargo)
(sup '(rust-mode :host github :repo "Linerre/rust-mode"))
(sup '(typst-ts-mode :type git :host sourcehut :repo "meow_king/typst-ts-mode"))
(sup '(zk :type git :host github :repo "localauthor/zk"))
(sup '(jstx :type git :host github :repo "llemaitre19/jtsx"))
(sup 'highlight-defined)
(sup 'symbol-overlay)
(sup 'vertico)
(sup 'vundo)
(sup 'envrc)

;;; Managed by pacman (Arch) or portage (Gentoo)
;; (sup '(meow :type git :host github :repo "DogLooksGood/meow")) installed via pacman/portage
;; (sup 'ebuild-mode)
;; (sup '(fpga :type git :host github :repo "gmlarumbe/fpga"))
;; (sup '(verilog-ts-mode :type git :host github :repo "gmlarumbe/verilog-ts-mode"))
;; (sup '(verilog-mode :type git :host github :repo "veripool/verilog-mode"))
;; (sup 'direnv)
;;; Dropped
;; (sup
;;  '(zenburn-emacs :type git :host github :repo "bbatsov/zenburn-emacs"))
;; (sup 'marginalia)

(provide 'init-packages)

;;; init-packages.el ends here
