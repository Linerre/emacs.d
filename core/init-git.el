;;; -*- lexical-binding: t -*-

;;; magit
(autoload #'magit "magit" nil t)
(autoload #'magit-status "magit" nil t)
(autoload #'magit-diff "magit" nil t)
(autoload #'magit-blame "magit" nil t)
(setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)

(with-eval-after-load "magit"
  ;; (require 'forge)
  (define-key transient-base-map (kbd "<escape>") #'transient-quit-one))

;; smerge-mode
(autoload 'smerge-mode "smerge-mode" nil t)

(defun sm-try-smerge ()
  (save-excursion
  	(goto-char (point-min))
  	(when (re-search-forward "^<<<<<<< " nil t)
  	  (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)

;; github token for forge
(setq auth-source '("~/.authinfo.gpg"))

(provide 'init-git)

;;; init-git ends here
