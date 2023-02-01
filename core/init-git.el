;;; -*- lexical-binding: t -*-

;;; magit
(autoload #'magit "magit" nil t)

(with-eval-after-load "magit"
  (define-key transient-base-map (kbd "<escape>") #'transient-quit-one))

(autoload #'magit-status "magit" nil t)
(autoload #'magit-diff "magit" nil t)
(autoload #'magit-blame "magit" nil t)

;; smerge-mode
(autoload 'smerge-mode "smerge-mode" nil t)

(defun sm-try-smerge ()
  (save-excursion
  	(goto-char (point-min))
  	(when (re-search-forward "^<<<<<<< " nil t)
  	  (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)

(provide 'init-git)
;;; init-git ends here
