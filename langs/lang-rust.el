;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Last Update: 2023-04-21
;;; For programming in Rust, choose either of the following bundles:
;;; 1. rust-mode + cargo/cargo-mode
;;; cargo mode offers keys to run cargo cmds within Emacs
;;; 2. rustic (based on rust-mode/util.el) and integrates cargo cmds

;;; Code:
(defun +hl-rust-kw ()
  "Highlight Rust docstrings in `rust-ts-mode' of Emacs 29."
  (font-lock-remove-keywords
   'rust-mode
   '(("\\([[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*!\\)[({[:space:][]"
      1 font-lock-preprocessor-face)))

  (font-lock-add-keywords 'rust-mode
                          '(("\\<\\(FIXME\\|TODO\\):" 1 font-lock-warning-face prepend)
                            ("\\<\\(true\\|false\\)" . font-lock-constant-face)
                            ("\\<\\([A-Z_]+\\>\\)" . font-lock-constant-face)
                            ("\\([[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*!\\)[({[:space:][]"
      1 font-lock-constant-face))))

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
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
;; (with-eval-after-load "rust-ts-mode"
;;   (add-hook 'rust-ts-mode-hook #'+rust-ts-setup)
;;   (add-hook 'rust-ts-mode-hook #'+hl-rust-kw)
;;   (add-hook 'rust-ts-mode-hook #'cargo-minor-mode)
;;   (add-hook 'rust-ts-mode-hook
;;             (lambda ()
;;               (local-set-key (kbd "C-c C-l") #'lsp)
;;               (local-set-key (kbd "C-c '") #'+rust-toggle-lsp-target)
;;               (local-set-key (kbd "C-c C-a") #'lsp-execute-code-action))))
;; ------------------- RUST-TS-MODE -----------------------

;; Tree-sitter until the built-in *-ts-mode matures
;; (scoped_identifier (identifier) @x)
;; (scoped_use_list (identifier) @x)
;; (mod_item (identifier) @x)

;; (meta_item (identifier) @id
;;            (#match? @id "^derive|allow|cfg"))

;; Map @rust.constant (identifier) capture to constant face.
;; (add-function :before-until tree-sitter-hl-face-mapping-function
;;               (lambda (capture-name)
;;                 (pcase capture-name
;;                   ("rust.constant" 'font-lock-constant-face))))

(with-eval-after-load "rust-mode"
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  ;; (add-hook 'rust-mode-hook #'tree-sitter-mode)
  ;; (add-hook 'rust-mode-hook #'tree-sitter-hl-mode)
  ;; (add-hook 'rust-mode-hook #'ts-fold-mode)
  (add-hook 'rust-mode-hook #'+hl-rust-kw)
  (add-hook 'rust-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c '") #'+rust-toggle-lsp-target)
              (local-set-key (kbd "C-c C-l") #'lsp)
              (local-set-key (kbd "C-c C-a") #'lsp-execute-code-action)))
  ;; (add-hook 'rust-mode-hook
  ;;           (lambda ()
  ;;             (tree-sitter-hl-add-patterns nil
  ;;               [(arguments (identifier) @rust.constant
  ;;                           (.match? @rust.constant "^[A-Z][A-Z_\\d]+"))
  ;;                (const_item (identifier) @rust.constant)
  ;;                (tuple_expression (identifier) @rust.constant)
  ;;                ])))
  )

(provide 'lang-rust)

;;; lang-rust.el ends here
