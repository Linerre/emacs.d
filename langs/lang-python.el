;; -*- lexical-binding: t -*-

(autoload #'python-mode "pyton" nil t)
(defun my/python-mode-tweaks ()
  (setq tab-width 4
        ;; make sure this is true, though it is by default
        electric-indent-inhibit t))

(with-eval-after-load "python"
  (setq python-indent-offset 4
        python-indent-guess-indent-offset-verbose nil)
  (add-hook 'python-mode-hook #'my/python-mode-tweaks))

(provide 'lang-python)
