;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Configurations for functional programming langs excpet lisp
;;; Basically, Haskell, Ocaml and Coq
;;; Code:

;; Haskell
(with-eval-after-load "haskell-mode"
  (add-hook 'haskell-mode-hook #'flymake-hlint-load)
  (add-hook 'haskell-mode-hook #'hindent-mode))

;; OCaml

;; Coq
;; Disable symbol prettification
(setq company-coq-disabled-features '(prettify-symbols))
;; Load company-coq when opening Coq files
(add-hook 'coq-mode-hook #'company-coq-mode)

(provide 'lang-fp)
;;; lang-fp.el ends here
