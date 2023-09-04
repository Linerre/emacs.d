;;; -*- lexical-binding: t -*-

;;; Commentary:
;;; Domain Specific Languages such as Nix, SQL, Tex, Markdown etc.

;;; -- Org --------------------------------
;; Use serif font for org mode body
(add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'org-mode-hook #'yas-minor-mode)

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
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("DONE" . org-done)
        ("DOING" . org-drawer)))

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
   '(org-latex-and-related ((t (:inherit 'fixed-pitch))))))

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

;;; -- SQL --------------------------------
(with-eval-after-load "sql"
  (add-hook 'sql-mode-hook 'sqlind-minor-mode)
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (define-key sql-mode-map (kbd "C-c u") 'sqlup-capitalize-keywords-in-region))

(with-eval-after-load "sqlup-mode"
  (add-to-list 'sqlup-blacklist "final")
  (add-to-list 'sqlup-blacklist "name")
  (add-to-list 'sqlup-blacklist "result"))

;;; -- COQ --------------------------------

;;; -- Tex --------------------------------
;; auctex
(setq TeX-auto-save t
      TeX-parse-self t
      TeX-open-quote ""
      TeX-close-quote ""
      ;; use pdflatex
      TeX-PDF-mode t)

(setq-default Tex-master nil)

(setq TeX-view-program-selection
      '((output-pdf "okular")
        (output-dvi "xdvi")
        (output-html "browser")))

(setq TeX-view-program-list
      '(("okular" "okular %o")
        ("browser" "chromium %o")))

;; LaTeX-mode is an alias for latex-mode
;; but AUCTeX calls `LaTeX-mode-hook' for any hooks
(with-eval-after-load 'tex-mode
  (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
  (add-hook 'LaTeX-mode-hook #'company-auctex-init)
  (add-hook 'LaTeX-mode-hook #'yas-minor-mode))

(with-eval-after-load 'bibtex
  (add-hook 'bibtex-mode-hook #'visual-line-mode)
  (add-hook 'bibtex-mode-hook #'yas-minor-mode))

;;; -- Markdown -----------------------------
(autoload #'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(autoload #'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)

(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(with-eval-after-load "markdown-mode"
  (add-hook 'markdown-mode-hook #'flyspell-mode))

(provide 'lang-dsl)

;;; lang-dsl.el ends here
