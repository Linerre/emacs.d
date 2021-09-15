(straight-use-package 'gkroam)
(add-hook 'org-mode-hook 'gkroam-mode)
(setq gkroam-root-dir "~/projects/gkroam/org/")
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
