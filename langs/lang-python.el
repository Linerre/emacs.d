;;; lang-python.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(autoload #'python-mode "python" nil t)

(defun +insert-utf8-header ()
  "Insert utf-8 header line to the beginning of a Python script."
  (interactive)
  (goto-char (point-min))
  (insert "# -*- coding: utf-8 -*-"))

(with-eval-after-load "python"
  (setq python-indent-offset 4
        python-indent-guess-indent-offset-verbose nil)
  (define-key python-mode-map (kbd "C-c u") #'+insert-utf8-header))

(provide 'lang-python)
;;; lang-python.el ends here
