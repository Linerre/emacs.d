;;; -*- lexical-binding: t -*-
;;; Last Modified: Sun Oct 10 01:07:46 2021
;;; Commentary:
;;; I switched back to org-roam; deprecated
(add-hook 'org-mode-hook 'gkroam-mode)
;; gkroam-root-dir must be "~/projects/gkroam/org/"?
(setq gkroam-root-dir "~/projects/gkroam/")
(setq gkroam-prettify-page-p t
      gkroam-show-brackets-p nil
      gkroam-use-default-filename t
      gkroam-window-margin 4)

(with-eval-after-load "gkroam"
  (define-key gkroam-mode-map (kbd "C-c r f") #'gkroam-find)
  (define-key gkroam-mode-map (kbd "C-c r I") #'gkroam-index)
  (define-key gkroam-mode-map (kbd "C-c r d") #'gkroam-daily)
  (define-key gkroam-mode-map (kbd "C-c r i") #'gkroam-insert)
  (define-key gkroam-mode-map (kbd "C-c r e") #'gkroam-link-edit)
  (define-key gkroam-mode-map (kbd "C-c r u") #'gkroam-show-unlinked)
  (define-key gkroam-mode-map (kbd "C-c r p") #'gkroam-toggle-prettify)
  (define-key gkroam-mode-map (kbd "C-c r t") #'gkroam-toggle-brackets)
  (define-key gkroam-mode-map (kbd "C-c r R") #'gkroam-rebuild-caches)
  (define-key gkroam-mode-map (kbd "C-c r g") #'gkroam-update))

(provide 'init-gkroam)
