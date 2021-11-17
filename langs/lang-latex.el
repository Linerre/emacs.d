;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(straight-use-package 'auctex)
(straight-use-package 'auctex-lua)
(straight-use-package 'auctex-latexmk)
(straight-use-package 'company-auctex)
(straight-use-package 'cdlatex)

(require 'smartparens-latex)

;; use PDF viewers depending on system type
(defun +which-pdf-viewer ()
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
          (output-dvi "xdvi")))))
;; auctex
(setq TeX-auto-save t
      TeX-parse-self t
      ;; use pdflatex
      TeX-PDF-mode t)
(setq-default Tex-master nil)

(autoload #'latex-mode "tex-mode" nil t)
(add-hook 'LaTeX-mode-hook #'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'global-linum-mode)
(add-hook 'LaTeX-mode-hook 'hl-line-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
(add-hook 'LaTeX-mode-hook #'company-auctex-init)
(add-hook 'LaTeX-mode-hook #'+which-pdf-viewer)

(provide 'lang-latex)
