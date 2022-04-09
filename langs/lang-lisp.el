;; -*- lexical-binding: t -*-

(sup 'highlight-defined)

;; LEXICAL
(defun lexical-binding ()
  (interactive)
  (goto-char (point-min))
  (insert ";;; -*- lexical-binding: t; -*-"))

(autoload #'highlight-defined-mode "highlight-defined" nil t)

;; elisp
(with-eval-after-load "elisp-mode"
  (add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode)
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (add-hook 'after-save-hook #'check-parens)))
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode))

(provide 'lang-lisp)

;; lang-list ends here
