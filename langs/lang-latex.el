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

(with-eval-after-load 'tex-mode
  (dolist (hook '(LaTeX-mode-hook))
          (add-hook hook 'turn-on-cdlatex)
          (add-hook hook #'company-auctex-init)
          (add-hook hook #'+which-pdf-viewer)
          (add-hook 'c-mode-common-hook 'electric-pair-mode)))

(with-eval-after-load 'bibtex
  (dolist (hook '(bibtex-mode-hook))
          (add-hook 'bibtex-mode-hook 'display-line-numbers-mode)
          (add-hook 'bibtex-mode-hook 'visual-line-mode)
          (add-hook 'bibtex-mode-hook 'hl-line-mode)
          (add-hook 'bibtex-mode-hook 'flyspell-mode)))

(provide 'lang-latex)

;; lang-latex ends here
