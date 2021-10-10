;;; -*- lexical-binding: t -*-
;; ----------------- ROAM ----------------
(setq
 org-roam-v2-ack t

 org-roam-directory
 (let ((ord (expand-file-name "~/projects/org")))
   (unless (file-directory-p ord) (make-directory ord))
   ord))

(add-hook 'org-mode-hook 'org-roam-mode)

(with-eval-after-load "org-roam"
  ;; https://www.orgroam.com/manual.html#Roam-Protocol
  (global-set-key (kbd "C-x n l") 'org-roam-buffer-toggle)
  (global-set-key (kbd "C-x n f") 'org-roam-node-find)
  (global-set-key (kbd "C-x n g") 'org-roam-graph)
  (global-set-key (kbd "C-x n i") 'org-roam-node-insert)
  (global-set-key (kbd "C-x n c") 'org-roam-capture)
  (global-set-key (kbd "C-x n s") 'org-roam-db-sync)

  (org-roam-setup)
  (require 'org-roam-protocol))

(provide 'init-org-roam)
;; init-org-roam ends here
