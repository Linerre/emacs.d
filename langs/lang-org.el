;;; -*- lexical-binding: t -*-

(with-eval-after-load 'org
  ;; VARS
  ;; (setq org-log-done 'time)
  (setq-default fill-column 80)
  (setq
   org-deadline-warning-days 0
   org-startup-folded 'content
   org-hide-leading-stars t
   org-hide-emphasis-markers t
   org-agenda-include-diary t
   org-src-fontify-natively t
   org-src-preserve-indentation t
   org-edit-source-content-indentation 0
   ;; defaults to 2
   org-indent-indentation-per-level 1)
  ;; (setq org-startup-indented t) should work the same
  (setq org-directory "~/projects/org")
  (setq org-agenda-files '("~/projects/org/calendar.org"))
  ;;			 "~/projects/org/projects.org"
  ;;			 "~/projects/org/reminder.org"))

  (setq org-tag-faces
        '(("work" . "#d65d0e")
          ("personal" . "#fe8019")
          ;; Gruvbox dark aqua
          ("literature" . "#427b58")
          ("fleeting" . "#a8b1c1")))
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
  (global-set-key (kbd "C-c l") 'org-link-store-props)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)

  ;; HOOKS
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

(provide 'lang-org)
