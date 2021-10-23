;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'smartparens-latex)

(setq TeX-auto-save t
      TeX-parse-self t
      ;; use pdflatex
      TeX-PDF-mode t)
(setq-default Tex-master nil)


(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
(add-hook 'latex-mode-hook 'turn-on-cdlatex)

(when *is-mac*
  (setq TeX-view-program-list
        '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")))
  (setq TeX-view-program-selection
        '((output-pdf "Skim")
          (output-dvi "xdvi"))))

(provide 'init-latex)
