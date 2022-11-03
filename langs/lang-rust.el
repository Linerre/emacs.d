;;; -*- lexical-binding: t; -*-
;;; Commentary
;;; Code:

(sup 'cargo-mode)
;; (sup 'rust-mode)
(sup 'rustic)

;; (with-eval-after-load "rust-mode"
;;   (add-hook 'rust-mode-hook 'eglot-ensure)
;;   (add-hook 'rust-mode-hook 'cargo-minor-mode)
;;   (define-key rust-mode-map (kbd "C-c C-r") #'rust-run)
;;   (define-key rust-mode-map (kbd "C-c C-t") #'rust-test)
;;   (define-key rust-mode-map (kbd "C-c c b") #'rust-compile)
;;   (define-key rust-mode-map (kbd "C-c c k") #'rust-check))

(with-eval-after-load "rustic"
  (add-hook 'rustic-mode-hook 'eglot-ensure)
  (add-hook 'rustic-mode-hook 'cargo-minor-mode)
  (setq rustic-lsp-client 'eglot))

(with-eval-after-load "cargo-mode"
  (setq compilation-scroll-output 'first-error))

(provide 'lang-rust)
;;; lang-rust.el ends here
