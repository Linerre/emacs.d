;;; flycheck/make.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;; Flymake
(autoload #'flymake-mode "flymake" nil t)
(global-set-key (kbd "C-c C-f m") #'flymake-mode)

(setq flymake-no-changes-timeout 2)
(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "C-c k") 'flymake-show-diagnostics-buffer)
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(provide 'init-linter)

;;; init-linter.el ends here
