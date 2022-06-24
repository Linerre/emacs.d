;;; -*- lexical-binding: t -*-

;;; Commentary:
;;; org templates config

;;; Code:

(setq org-todo-keyword-faces
      '(("CANCELLED" . (:foreground "#a89984"))
        ("DOING" . (:foreground "#d79921"))))

;;; ORG CAPTURE
(setq org-capture-templates
      '(("t" "Tasks")
        ("td" "With Dealines" entry     ; todos->deadline
	       (file+headline "tasks.org" "Deadlines")
	       "* TODO %? %^G\nDEADLINE: %T" :kill-buffer t :prepend t)
        ("ts" "Scheduled" entry         ; todos->scheduled
	       (file+headline "tasks.org" "TODOS")
	       "* TODO %? %^G\nSCHEDULED: %T" :kill-buffer t :prepend t)
        ;; notes
        ("n" "Daily Notes" entry
         (file+headline "notes.org" "NOTES")
         "* %? %^G\n%U" :kill-buffer t :prepend t)
        ;; readings
        ("r" "Readings")
        ("ra" "Article" entry
         (file+headline "readings.org" "ARTICLE")
         "* TODO %^{Short title for heading} %^G \nTTL: %^{Long title} \nSRC: %^{Link} \n%u \n%^{Quick note?}"
         :kill-buffer t)
        ("rb" "Book" entry
         (file+headline "readings.org" "BOOK")
         "* TODO %^{Short title for heading} %^G \nTTL: %^{Long title} \nSRC: %^{Link} \n%u \n%^{Quick note?}"
         :kill-buffer t)
        ))

;; (add-to-list 'org-capture-templates
;;              '("b" "Billing" plain
;;                (file+function "~/projects/org/bills.org" find-month-tree)
;;                " | %U | %^{Type} | %^{Detail} | %^{Amount} |" :kill-buffer t))

(setq org-refile-targets '(("~/projects/org/inbox.org" :maxlevel . 3)
                           ("~/projects/org/done.org" :level . 1)
                           ("~/projects/org/projects.org" :maxlevel . 2)
		                       ("~/projects/org/readings.org" :maxlevel . 1)
		                       ("~/projects/org/barn.org" :maxlevel . 5)))


(setq org-refile-use-outline-path 'file
      org-log-refile t)

(provide 'lang-orgtemp)
;;; lang-orgtemp ends here
