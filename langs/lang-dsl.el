;;; -*- lexical-binding: t -*-

;;; Commentary:
;;; Domain Specific Languages such as Nix, SQL, Tex, Markdown etc.

;;; -- Org --------------------------------
;; (add-hook 'org-mode-hook #'variable-pitch-mode)
;; (custom-set-faces
;;    '(org-table ((t :inherit 'fixed-pitch)))
;;    '(org-code ((t :inherit 'fixed-pitch)))
;;    '(org-block ((t :inherit 'fixed-pitch)))
;;    '(org-checkbox ((t :inherit 'fixed-pitch :background unspecified :box unspecified)))
;;    '(org-latex-and-related ((t (:inherit 'fixed-pitch)))))

(setq
 org-deadline-warning-days 0
 org-hide-leading-stars t
 ;; org-startup-folded 'content
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

(setq org-html-doctype "html5")
(add-hook 'org-mode-hook (lambda ()
                             (modify-syntax-entry ?> "." org-mode-syntax-table)
                             (modify-syntax-entry ?< "." org-mode-syntax-table)))
(setq org-confirm-babel-evaluate nil)
(with-eval-after-load 'org
  (add-to-list 'org-file-apps '("\\.pdf::\\([0-9]+\\)\\'" . "okular -p %1 %s"))
  (define-key org-mode-map (kbd "C-c A") 'org-agenda)
  (define-key org-mode-map (kbd "C-c c") 'org-capture)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell . t)
       (clojure . t))))

;;; Nix
(autoload #'nix-mode "nix-mode")
(with-eval-after-load "nix-mode"
  (add-hook 'nix-mode-hook #'electric-pair-mode))

;;; -- SQL --------------------------------
(with-eval-after-load "sql"
  (add-hook 'sql-mode-hook 'sqlind-minor-mode)
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (define-key sql-mode-map (kbd "C-c M-u") 'sqlup-capitalize-keywords-in-region))

(with-eval-after-load "sqlup-mode"
  (add-to-list 'sqlup-blacklist "final")
  (add-to-list 'sqlup-blacklist "name")
  (add-to-list 'sqlup-blacklist "result"))

;;; -- COQ --------------------------------

;;; -- Tex --------------------------------
(defun +mark-gloassary-word ()
  "Mark the word at point as gloassary and into the format `\gls{WORD-AT-PT}',
  move to `vocabs' buffer and insert the old word surrounded by a LaTeX command."
  (interactive)
  (save-excursion
    (let ((gls (thing-at-point 'word))
          (vocab-file "vocabs.tex"))
      (if gls
          (progn
            (delete-region (car (bounds-of-thing-at-point 'word))
                           (cdr (bounds-of-thing-at-point 'word)))
            (insert (format "\\gls{%s}" gls))
            (kill-new gls)
            (message "Marked '%s' as '\\gls{%s}'" gls gls)
            (if (file-exists-p vocab-file)
                (find-file-other-window vocab-file nil)
              (find-file-other-window (find-file-noselect file-path nil nil nil))))
          (message "No word at point")))))

(defun +replace-puncs ()
  "Replace full-width punctuations with half-width ones in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[“”‘’—]" nil t)
      (replace-match (pcase (match-string 0 nil)
                       ("“" "``")
                       ("”" "''")
                       ("‘" "`'")
                       ("’" "'")
                       ("—" "---"))))))

;; Map *.tex files to LaTeX-mode (AUCTeX)
(add-to-list 'auto-mode-alist '("\\.[tT]e[xX]\\'" . LaTeX-mode))

;; disable raise/lower scripts
(setq tex-fontify-script nil
      font-latex-fontify-script nil)

;; auctex
(setq TeX-auto-save t
      TeX-parse-self t
      TeX-open-quote ""
      TeX-close-quote ""
      ;; use pdflatex
      TeX-PDF-mode t)

(setq-default Tex-master "master")

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
  ;; (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
  (add-hook 'LaTeX-mode-hook #'company-mode)
  (add-hook 'LaTeX-mode-hook #'company-auctex-init)
  (add-hook 'LaTeX-mode-hook #'electric-pair-local-mode)
  (add-hook 'LaTeX-mode-hook #'yas-minor-mode)
  (add-hook 'LaTeX-mode-hook #'outline-minor-mode)
  (add-hook 'tex-mode-hook #'yas-minor-mode)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (keymap-local-set "C-c g" #'+mark-gloassary-word)
                               (keymap-local-set "C-c p" #'+replace-puncs)))
  ;; (define-key 'LaTeX-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
  ;; (define-key 'LaTeX-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)
  )

(with-eval-after-load 'bibtex
  (add-hook 'bibtex-mode-hook #'visual-line-mode)
  (add-hook 'bibtex-mode-hook #'yas-minor-mode)
  (add-hook 'bibtex-mode-hook #'electric-pair-local-mode))

;;; -- zk -----------------------------------
(setq zk-directory "~/projects/zknotes")
(setq zk-file-extension "md")
(setq zk-file-name-separator "-")
(zk-setup-embark)
(with-eval-after-load "zk"
(zk-setup-auto-link-buttons))

;; (require 'zk-consult)
;; (setq zk-tag-grep-function #'zk-consult-grep-tag-search
;;       zk-grep-function #'zk-consult-grep)

;;; -- Markdown -----------------------------
(autoload #'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(autoload #'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)

(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(with-eval-after-load "markdown-mode"
  (add-hook 'markdown-mode-hook #'electric-pair-local-mode))

(autoload #'typst-ts-mode "typst-ts-mode" "Major mode for typst using treesit" t)
(add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode))
(with-eval-after-load "typst-ts-mode"
  (add-hook 'typst-ts-mode-hook #'electric-pair-local-mode))


(provide 'lang-dsl)

;;; lang-dsl.el ends here
