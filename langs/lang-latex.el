;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(straight-use-package 'auctex)
(straight-use-package 'auctex-lua)
(straight-use-package 'auctex-latexmk)
(straight-use-package 'company-auctex)
(straight-use-package 'cdlatex)

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
(when *is-linux*
  (setq TeX-view-program-list
        '(("Zathura" "zathura %o")))
  (setq TeX-view-program-selection
        '((output-pdf "Zathura")
          (output-dvi "xdvi"))))
(provide 'lang-latex)
