;; -*- lexical-binding: t -*-


(setq python-indent-offset 4
      python-indent-guess-indent-offset-verbose nil)

(defun my/python-mode-tweaks ()
  (setq tab-width 4
        ;; make sure this is true, though it is by default
        electric-indent-inhibit t))

(add-hook 'python-mode-hook #'my/python-mode-tweaks)
(provide 'init-python)
