;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Last Update: 2023-04-21
;;; For programming in Rust, choose either of the following bundles:
;;; 1. rust-mode + cargo/cargo-mode
;;; cargo mode offers keys to run cargo cmds within Emacs
;;; 2. rustic (based on rust-mode/util.el) and integrates cargo cmds

;;; Code:

(defun +rust-toggle-lsp-target ()
  "Toggle target to be compiled by rustc."
  (interactive)
  (require 'lsp-rust)
  (when
      (y-or-n-p (format "Current target is [%s], switch?"
                        (or lsp-rust-analyzer-cargo-target "default")))
    (if lsp-rust-analyzer-cargo-target
        (setq lsp-rust-analyzer-cargo-target nil
              cargo-process--command-check "check")
      (setq lsp-rust-analyzer-cargo-target "wasm32-unknown-unknown"
            cargo-process--command-check "check --target wasm32-unknown-unknown"))))

;; ------------------- RUST-TS-MODE -----------------------
;; (defun +rust-ts-setup ()
;;   "Set up `rust-ts-mode' with the given treesit features."
;;   (treesit-font-lock-recompute-features '(bracket) '(type assignment keyword variable)))

(with-eval-after-load "rust-mode"
  (setq lsp-rust-analyzer-closing-brace-hints nil
        lsp-rust-analyzer-cargo-watch-enable nil
        lsp-rust-analyzer-highlight-exit-points nil
        lsp-rust-analyzer-highlight-references nil
        lsp-rust-analyzer-highlighting-strings nil
        lsp-rust-analyzer-proc-macro-enable nil
        lsp-rust-analyzer-server-format-inlay-hints nil)

  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'rust-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c '") #'+rust-toggle-lsp-target)
              (local-set-key (kbd "C-c C-l") #'lsp)
              (local-set-key (kbd "C-c C-a") #'lsp-execute-code-action))))

;; Move
(with-eval-after-load "move-mode"
  ;; (setq move-bin "sui move")
  (define-key move-mode-map (kbd"C-c C-l") #'lsp))

(provide 'lang-rust)

;;; lang-rust.el ends here
