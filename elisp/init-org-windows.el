;;; -*- lexical-binding: t -*-
;;; load this configuration for org on windows

;;(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c l") 'org-link-store-props)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq
 org-deadline-warning-days 0
 org-startup-folded 'content
 org-hide-leading-stars t
 org-agenda-include-diary t
 org-src-fontify-natively t)

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

;; on windows always take fleeting notes
;; org them on linux and macos
(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'yas-minor-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (org-indent-mode t))


(provide 'init-org-windows)
;; init-org-windows ends here
