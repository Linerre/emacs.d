;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
  (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (add-hook 'after-save-hook #'check-parens)))
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode))

;; scheme
(setq scheme-program-name "mit-scheme")

(provide 'lang-lisp)

;;; lang-lisp.el ends here
