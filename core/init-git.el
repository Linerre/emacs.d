;;; -*- lexical-binding: t -*-
(emerge 'magit)

;;; magit
(autoload #'magit "magit" nil t)

(with-eval-after-load "magit"
  (define-key transient-base-map (kbd "<escape>") #'transient-quit-one))

(autoload #'magit-status "magit" nil t)
(autoload #'magit-diff "magit" nil t)
(autoload #'magit-blame "magit" nil t)

(provide 'init-git)
;;; init-git ends here
