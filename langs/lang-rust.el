;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Last Update: 2023-01-09
;;; For programming in Rust, choose either of the following bundles:
;;; 1. rust-mode + cargo-mode
;;; cargo mode offers keys to run cargo cmds within Emacs
;;; 2. rustic (based on rust-mode/util.el) and integrates cargo cmds

;;; Code:

;; (with-eval-after-load "rust-mode"
;;   (add-hook 'rust-mode-hook 'eglot-ensure)
;;   (add-hook 'rust-mode-hook 'cargo-minor-mode)
;;   (define-key rust-mode-map (kbd "C-c C-r") #'rust-run)
;;   (define-key rust-mode-map (kbd "C-c C-t") #'rust-test)
;;   (define-key rust-mode-map (kbd "C-c c b") #'rust-compile)
;;   (define-key rust-mode-map (kbd "C-c c k") #'rust-check))

(with-eval-after-load "rustic"
  ;; (setq rustic-lsp-client 'eglot)
  ;; (add-hook 'rustic-mode-hook 'eglot-ensure)
  ;; rustic provides cmds to interact with cargo
  (add-hook 'rustic-mode-hook 'lsp))

(provide 'lang-rust)
;;; lang-rust.el ends here
