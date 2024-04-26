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
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (add-hook 'after-save-hook #'check-parens)))
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode))

;; scheme
;; (setq scheme-program-name "mit-scheme")
;; (setq scheme-program-name "csi -:c")
(setq scheme-program-name "guile")
(with-eval-after-load "scheme-mode"
  (add-hook 'scheme-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'geiser-mode)
  (define-key scheme-mode-map (kbd "C-c g") #'geiser))

(with-eval-after-load "geiser-repl"
  (add-hook 'geiser-repl-mode-hook #'paredit-mode))


(provide 'lang-lisp)

;;; lang-lisp.el ends here
