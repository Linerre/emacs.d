;; -*- lexical-binding: t -*-

;;(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c l") 'org-link-store-props)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;;; ------------------- ORG MODE ---------------------
;;; Consider using Google Calendar directly
;;; TODO
;;; 1) connect org agenda to google calendar
;;; 2) sync agenda on laptop to iOS google calendar
;; org-mode keys
(setq org-agenda-files '("~/projects/gkroam/gcal.org"))
;;			 "~/projects/gkroam/org/projects.org"
;;			 "~/projects/gkroam/org/reminder.org"))


;; I don't want this becasue every time I change a state
;; from TODO-state, it will insert the CLOSED timestamp
;; (setq org-log-done 'time)

;; try it out on Emacs 27.1
;; see https://brantou.github.io/2017/03/21/just-try/
(setq
 org-deadline-warning-days 0
 org-startup-folded 'content
 org-hide-leading-stars t
 org-agenda-include-diary t
 org-src-fontify-natively t)

;; org-mode to do
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DOING(n)" "DONE(d)")
	(sequence "CANCELLED(c@)" "|" "EVENT(e)" "IDEA(a)" "WATCH(w)")
;; right arrow: migrate to Futher; left arrow: migrate to Other collections
	(sequence "✝TODO(i)" "|" ">(f)" "<(o)")))

(setq org-todo-keyword-faces
      '(("CANCELLED" . (:foreground "#a89984"))
        ("DOING" . (:foreground "#d79921"))))

(setq org-tag-faces
      '(("work" . "#d65d0e")
	("personal" . "#fe8019")))


;;; --------------------------------------------------------------
;;; -------------------- ORG CAPTURE -----------------------------
;;; --------------------------------------------------------------
(setq org-capture-templates nil)

;; inbox-tasks
(add-to-list 'org-capture-templates
	     '("t" "Inbox [TODO]" entry
	       (file+headline "~/projects/gkroam/org/inbox.org" "Tasks")
	       "* TODO %? %^G" :kill-buffer t))

;; inbox-idea/note/thoughts
(add-to-list 'org-capture-templates
	     '("a" "Inbox [IDEA]" entry
	       (file+headline "~/projects/gkroam/org/inbox.org" "Thoughts")
              "* IDEA %? \n%u" :kill-buffer t))

;; inbox-event
(add-to-list 'org-capture-templates
     '("e" "Inbox [EVENTS]" entry
	       (file+headline "~/projects/gkroam/org/inbox.org" "Events")
               "* EVENT %? %U" :kill-buffer t))

;; inbox-note
(add-to-list 'org-capture-templates
     '("n" "Inbox [NOTE]" entry
       (file+headline "~/projects/gkroam/org/inbox.org" "Thoughts")
       "* NOTE %? %^G" :kill-buffer t))

;; inbox-reading
(add-to-list 'org-capture-templates '("r" "Reading"))
(add-to-list 'org-capture-templates
     '("rb" "Readings [Book]" entry
       (file+headline "~/projects/gkroam/org/readings.org" "Book")
               "* TODO Title: %^{Title} \nSource: %^{Link} \n%u\n" :kill-buffer t))
(add-to-list 'org-capture-templates
     '("ra" "Readings [Article]" entry
       (file+headline "~/projects/gkroam/org/readings.org" "Article")
               "* TODO [[%^{Link}][%^{Title}]] \n%u\n" :kill-buffer t))

;; reminder
(add-to-list 'org-capture-templates
             '("T" "Tickler" entry
      (file+headline "~/projects/gkroam/org/reminder.org" "Tickler")
              "* TODO %? %^G" :kill-buffer t))

(add-to-list 'org-capture-templates
             '("b" "Billing" plain
               (file+function "~/projects/gkroam/org/bills.org" find-month-tree)
               " | %U | %^{Type} | %^{Detail} | %^{Amount} |" :kill-buffer t))

(setq org-refile-targets '(("~/projects/gkroam/org/inbox.org" :maxlevel . 3)
                           ("~/projects/gkroam/org/reminder.org" :level . 1)
                           ("~/projects/gkroam/org/projects.org" :maxlevel . 2)
		   ("~/projects/gkroam/org/readings.org" :maxlevel . 1)
		   ("~/projects/gkroam/org/barn.org" :maxlevel . 5)))


(setq org-refile-use-outline-path 'file
      org-log-refile t)

(add-hook 'org-mode-hook 'yas-minor-mode)

;;; ----------------- GKROAM ----------------
(require 'init-gkroam)
;;; ------------------- ORG GCAL ---------------------
;;; Must use full path?
(when (file-exists-p "~/.emacs.d/elisp/init-org-gcal.el")
     (require 'init-org-gcal))


;;; --------------------------------------------------
;;; ------------------- ORG BABEL --------------------
;;; --------------------------------------------------
(require 'org-tempo)
(setq org-src-fontify-natively t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lua . t)
   (python . t)
   (C . t)
   (sqlite . t)
   (latex . t)
   (emacs-lisp . t)))

(provide 'init-org)
