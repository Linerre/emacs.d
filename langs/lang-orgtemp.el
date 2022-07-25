;;; -*- lexical-binding: t -*-

;;; Commentary:
;;; org templates config

;;; Code:

(setq org-todo-keyword-faces
      '(("CANCELLED" . (:foreground "#a89984"))
        ("DOING" . (:foreground "#d79921"))))

  ;; (setq org-tag-faces
  ;;       '(("work" . "#d65d0e")
  ;;         ("personal" . "#fe8019")
  ;;         ;; Gruvbox dark aqua
  ;;         ("literature" . "#427b58")
  ;;         ("fleeting" . "#a8b1c1")))

;;; ORG CAPTURE
(setq org-capture-templates
      '(("t" "Tasks")
        ("td" "With Dealines" entry     ; todos->deadline
	       (file+headline "tasks.org" "Deadlines")
	       "* TODO %? %^g\nDEADLINE: %T" :kill-buffer t :prepend t)
        ("ts" "Scheduled" entry         ; todos->scheduled
	       (file+headline "tasks.org" "TODOS")
	       "* TODO %? %^g\nSCHEDULED: %T" :kill-buffer t :prepend t)
        ;; memos
        ("m" "Memos")
        ("mt" "Twitter" entry
         (file "twitter.org")
         "* %^{Short Title} %^g \n*TWEET*: %^{Link} \n*TIME*: %u"
         :kill-buffer t :prepend t)
        ;; notes
        ("n" "Daily Notes" entry
         (file+headline "notes.org" "NOTES")
         "* %? %^g\n%U" :kill-buffer t :prepend t)
        ;; people
        ("p" "People")
        ("pa" "Academia" entry
         (file+headline "people.org" "Academia")
         "* %^{Full Name}%? \n%^{Association}p%^{Site}p%^{Title}p"
         :kill-buffer t :prepend t)
        ("pi" "Industry" entry
         (file+headline "people.org" "Industry")
         "* %^{Full Name}%? \n%^{Site}p"
         :kill-buffer t :prepend t)
        ;; readings
        ("r" "Readings")
        ("ra" "Article" entry
         (file+headline "readings.org" "ARTICLE")
         "* %^{Short title} %^g \n*TTL*: %^{Long title} \n*SRC*: %^{Link} \n*SUM*: %^{Summary} \n%u"
         :kill-buffer t)
        ("rb" "Book" entry
         (file+headline "readings.org" "BOOK")
         "* %^{Short title} %^g \n*TTL*: %^{Long title} \n*SRC*: %^{Link} \n*SUM*: %^{Summary} \n%u"
         :kill-buffer t)))

(setq org-refile-targets '(("~/projects/org/inbox.org" :maxlevel . 3)
                           ("~/projects/org/done.org" :level . 1)
                           ("~/projects/org/projects.org" :maxlevel . 2)
		                       ("~/projects/org/readings.org" :maxlevel . 1)
		                       ("~/projects/org/barn.org" :maxlevel . 5)))


(setq org-refile-use-outline-path 'file
      org-log-refile t)

(provide 'lang-orgtemp)
;;; lang-orgtemp ends here
