;;; -*- lexical-binding: t; -*-
;;; Commentary
;;; Code:
(sup 'rust-mode)

(with-eval-after-load "rust-mode"
  (add-hook 'rust-mode-hook #'electric-pair-mode)
  (define-key rust-mode-map (kbd "C-c C-r") #'rust-run)
  (define-key rust-mode-map (kbd "C-c C-t") #'rust-test)
  (define-key rust-mode-map (kbd "C-c c b") #'rust-compile)
  (define-key rust-mode-map (kbd "C-c c k") #'rust-check))

(provide 'lang-rust)
;;; lang-rust.el ends here
