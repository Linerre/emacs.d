;;; -*- lexical-binding: t -*-
;; ----------------- ROAM ----------------
(setq
 org-roam-v2-ack t

 org-roam-directory
 (let ((ord (expand-file-name "~/projects/org")))
   (unless (file-directory-p ord) (make-directory ord))
   ord))

(with-eval-after-load "org-roam"
  ;; https://www.orgroam.com/manual.html#Roam-Protocol
  (global-set-key (kbd "C-x M-n l") 'org-roam-buffer-toggle)
  (global-set-key (kbd "C-x M-n f") 'org-roam-node-find)
  (global-set-key (kbd "C-x M-n g") 'org-roam-graph)
  (global-set-key (kbd "C-x M-n i") 'org-roam-node-insert)
  (global-set-key (kbd "C-x M-n c") 'org-roam-capture)
  (global-set-key (kbd "C-x M-n s") 'org-roam-db-sync)

  (org-roam-setup)
  (require 'org-roam-protocol))

(require 'org-roam)

(provide 'init-org-roam)