;;; -*- lexical-binding: t -*-

;;; Commentary:
;;; Domain Specific Languages such as Nix, SQL, Tex, Markdown etc.

(sup 'auctex)
(sup 'auctex-lua)
(sup 'auctex-latexmk)
(sup 'company-auctex)
(sup 'cdlatex)
(sup 'markdown-mode)
(sup 'nix-mode)
(sup
 '(emacs-sql-indent :type git :host github :repo "alex-hhh/emacs-sql-indent"))
(sup
 '(sqlup-mode :type git :host github :repo "Trevoke/sqlup-mode.el"))

;;; Org
(with-eval-after-load 'org
  ;; (setq org-log-done 'time)
  (setq-default fill-column 80)
  (setq
   org-deadline-warning-days 0
   org-startup-folded 'content
   org-hide-leading-stars t
   ;; org-hide-emphasis-markers t
   org-agenda-include-diary t
   org-src-fontify-natively t
   org-src-preserve-indentation t
   org-edit-source-content-indentation 0
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

  ;; OTHER CONFIG
  (when (file-exists-p "~/.emacs.d/langs/lang-orgtemp.el")
    (require 'lang-orgtemp))

  ;; KEYBINDINGS
  (define-key org-mode-map (kbd "C-c l") 'org-link-store-props)
  (define-key org-mode-map (kbd "C-c A") 'org-agenda)
  (define-key org-mode-map (kbd "C-c c") 'org-capture)

  ;; HOOKS
  ;; TODO: disbale electric-pair-mode and use smartparens instead
  (dolist (h '(org-mode-hook))
    (add-hook h #'yas-minor-mode)
    (add-hook h #'visual-line-mode)
    (add-hook h #'display-fill-column-indicator-mode)
    (add-hook h #'org-indent-mode)))

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
