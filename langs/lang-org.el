;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'org
  ;; VARS
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
   ;; org-agenda-files '("~/projects/org/agenda.org")
   org-agenda-files (concat org-directory "/agenda.org")
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
    (add-hook h #'org-indent-mode)
    (add-hook h (lambda () (electric-pair-mode -1)))))

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

(provide 'lang-org)
