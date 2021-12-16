;; -*- lexical-binding: t; -*-

(autoload #'python-mode "python" nil t)

(defun +insert-utf8-header ()
  (interactive)
  (goto-line 1)
  (insert "# -*- coding: utf-8 -*-"))

(with-eval-after-load "python"
  (setq python-indent-offset 4
        python-indent-guess-indent-offset-verbose nil)
  (define-key python-mode-map (kbd "C-c u") #'+insert-utf8-header))

(provide 'lang-python)
;; lang-python ends here
