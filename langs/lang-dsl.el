;;; -*- lexical-binding: t -*-

;;; Commentary:
;;; Domain Specific Languages such as Nix, SQL, Tex, Markdown etc.

;;; Org
;; Use serif font for org mode body
(add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'org-mode-hook #'yas-minor-mode)
(add-hook 'org-mode-hook #'org-indent-mode)
;; HOOKS
;; It revent electric-pair from inserting `>' to match `<', but
;; it wont prevent ep from thinking `<' and `>' are matched
;; need to turn off check-paren
;; https://www.topbug.net/blog/2016/09/29/emacs-disable-certain-pairs-for-electric-pair-mode/
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local electric-pair-inhibit-predicate
                        `(lambda (c)
                           (if (char-equal c ?<)
                               t
                             (,electric-pair-inhibit-predicate c))))))

(setq
 org-deadline-warning-days 0
 org-startup-folded 'content
 org-hide-leading-stars t
 ;; org-hide-emphasis-markers t
 org-agenda-include-diary t
 org-src-fontify-natively t
 org-src-preserve-indentation t
 org-edit-source-content-indentation 0
 org-fontify-quote-and-verse-blocks t
 ;; defaults to 2
 ;; (setq org-startup-indented t) should work the same
 org-indent-indentation-per-level 1
 org-directory "~/projects/org"
 org-agenda-files '("~/projects/org/agenda.org")
 org-default-notes-file (concat org-directory "/notes.org"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DOING(n)" "DONE(d)")
	      (sequence "CANCELLED(c@)" "|" "EVENT(e)" "IDEA(a)" "WATCH(w)")
        ;; right arrow: migrate to Futher;
        ;; left arrow: migrate to Other collections
	      (sequence "✝TODO(i)" "|" ">(f)" "<(o)")))

(with-eval-after-load 'org
  (add-to-list 'org-file-apps '("\\.pdf::\\([0-9]+\\)\\'" . "okular -p %1 %s"))
  (define-key org-mode-map (kbd "C-c A") 'org-agenda)
  (define-key org-mode-map (kbd "C-c c") 'org-capture)

  ;; Use fixed pitch for table and code
  (custom-set-faces
   '(org-table ((t :inherit 'fixed-pitch)))
   '(org-code ((t :inherit 'fixed-pitch)))
   '(org-block ((t :inherit 'fixed-pitch)))
   '(org-checkbox ((t :inherit 'fixed-pitch :background unspecified :box nil)))
   '(org-latex-and-related ((t (:inherit 'fixed-pitch)))))
 )

;;; ORG BABEL
;(require 'org-tempo)
;(setq org-src-fontify-natively t)
;(org-babel-do-load-languages
; 'org-babel-load-languages
; '((lua . t)
;   (python . t)
;   (C . t)
;   (sqlite . t)
;   (latex . t)
;   (emacs-lisp . t)))

;;; Nix
(autoload #'nix-mode "nix-mode")
(with-eval-after-load "nix-mode"
  (add-hook 'nix-mode-hook #'electric-pair-mode))

;;; SQL
(with-eval-after-load "sql"
  (add-hook 'sql-mode-hook 'sqlind-minor-mode)
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (define-key sql-mode-map (kbd "C-c u") 'sqlup-capitalize-keywords-in-region))

;;; Tex
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
          (add-hook hook #'+which-pdf-viewer))

  (define-key latex-mode-map (kbd "C-c c w") #'count-words))

(with-eval-after-load 'bibtex
  (dolist (hook '(bibtex-mode-hook))
          (add-hook 'bibtex-mode-hook 'display-line-numbers-mode)
          (add-hook 'bibtex-mode-hook 'visual-line-mode)
          (add-hook 'bibtex-mode-hook 'hl-line-mode)
          (add-hook 'bibtex-mode-hook 'flyspell-mode)))

;;; Markdown
(autoload #'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(autoload #'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)

(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(with-eval-after-load "markdown-mode"
  (add-hook 'markdown-mode-hook #'flyspell-mode))

(provide 'lang-dsl)

;;; lang-dsl.el ends here
