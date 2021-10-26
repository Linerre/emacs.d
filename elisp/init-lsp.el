;;; -*- lexical-binding: t -*-
;;; Commentary:
;; see https://github.com/joaotavora/eglot/issues/518#issuecomment-664858272
;; project.el is a dependency for eglot
;; The current workaround is to load-library project manully

;;(load-library "project")
(straight-use-package 'eglot)

;;; flymake
(autoload #'flymake-mode "flymake" nil t)

(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "C-c C-d") 'flymake-show-diagnostics-buffer)
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))


;;; eglot
(setq
 eglot-stay-out-of nil
 ;; do not highlight symbol under cursor
 eglot-ignored-server-capabilites '(:documentHighlightProvider))

(autoload #'eglot-ensure "eglot" nil t)
;; TODO: python3 needs to be installed by nix
(with-eval-after-load "eglot"
  (add-to-list 'eglot-server-programs
               '(python-mode "/Users/errelin/Library/Python/3.9/bin/pyls"))
  ;; keybindings recommended in README of elgot
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c h") 'eldoc)
  (define-key eglot-mode-map (kbd "<f6>") 'xref-find-definitions))

(add-hook 'python-mode-hook 'eglot-ensure)

(provide 'init-lsp)
