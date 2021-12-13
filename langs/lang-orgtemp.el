;;; -*- lexical-binding: t -*-

;;; Commentary:
;;; org templates config

;;; Code:

(setq org-todo-keyword-faces
      '(("CANCELLED" . (:foreground "#a89984"))
        ("DOING" . (:foreground "#d79921"))))

;;; ORG CAPTURE
(setq org-capture-templates nil)

;; inbox-tasks
(add-to-list 'org-capture-templates
	     '("t" "Inbox [TODO]" entry
	       (file+headline "~/projects/org/inbox.org" "Tasks")
	       "* TODO %? %^G" :kill-buffer t))

;; inbox-idea/note/thoughts
(add-to-list 'org-capture-templates
	     '("a" "Inbox [IDEA]" entry
	       (file+headline "~/projects/org/inbox.org" "Thoughts")
              "* IDEA %? \n%u" :kill-buffer t))

;; inbox-event
(add-to-list 'org-capture-templates
     '("e" "Inbox [EVENTS]" entry
	       (file+headline "~/projects/org/inbox.org" "Events")
               "* EVENT %? %U" :kill-buffer t))

;; inbox-note
(add-to-list 'org-capture-templates
     '("n" "Inbox [NOTE]" entry
       (file+headline "~/projects/org/inbox.org" "Thoughts")
       "* NOTE %? %^G" :kill-buffer t))

;; inbox-reading
(add-to-list 'org-capture-templates '("r" "Reading"))
(add-to-list 'org-capture-templates
     '("rb" "Readings [Book]" entry
       (file+headline "~/projects/org/readings.org" "Book")
               "* TODO Title: %^{Title} \nSource: %^{Link} \n%u\n" :kill-buffer t))
(add-to-list 'org-capture-templates
     '("ra" "Readings [Article]" entry
       (file+headline "~/projects/org/readings.org" "Article")
               "* TODO [[%^{Link}][%^{Title}]] \n%u\n" :kill-buffer t))

;; reminder
(add-to-list 'org-capture-templates
             '("T" "Tickler" entry
      (file+headline "~/projects/org/reminder.org" "Tickler")
              "* TODO %? %^G" :kill-buffer t))

(add-to-list 'org-capture-templates
             '("b" "Billing" plain
               (file+function "~/projects/org/bills.org" find-month-tree)
               " | %U | %^{Type} | %^{Detail} | %^{Amount} |" :kill-buffer t))

(setq org-refile-targets '(("~/projects/org/inbox.org" :maxlevel . 3)
                           ("~/projects/org/reminder.org" :level . 1)
                           ("~/projects/org/projects.org" :maxlevel . 2)
		   ("~/projects/org/readings.org" :maxlevel . 1)
		   ("~/projects/org/barn.org" :maxlevel . 5)))


(setq org-refile-use-outline-path 'file
      org-log-refile t)

(provide 'lang-orgtemp)
;;; lang-orgtemp ends here
