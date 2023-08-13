;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun +complete ()
  "Expand snippet when there is one; otherwise, fall back on company."
  (interactive)
  (or (yas/expand)
      (company-indent-or-complete-common nil)))

;; ;;; eglot
;; (autoload 'eglot-ensure "eglot" nil nil)
;;
;; (defun +eglot-hook ()
;;   (setq eldoc-documentation-functions
;;         (cons #'flymake-eldoc-functions
;;               (remove #'flymake-eldoc-function eldoc-documentation-functions))))
;;
;; (with-eval-after-load "eglot"
;;   (add-hook 'eglot-managed-mode #'+eglot-hook)
;;   (setq eldoc-echo-area-use-multiline-p 3
;;         eldoc-echo-area-display-truncation-message nil)
;;   (set-face-attribute 'eglot-highlight-symbol-face nil
;;                       :background "#B3D7FF"))

;;; LSP
(autoload 'lsp "lsp-mode" nil t)

;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(setq lsp-keymap-prefix "C-c l"
      lsp-enable-symbol-highlighting nil
      lsp-enable-dap-auto-configure nil
      lsp-modeline-diagnostics-enable nil ; 5/6/7 <----- a string whose colors default to error/success
      lsp-lens-enable nil
      lsp-headerline-breadcrumb-enable nil
      lsp-signature-doc-lines 3)

(with-eval-after-load "lsp-mode"
  ;; start lsp after `modes' are enabled
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'typescript-ts-mode-hook #'lsp))

;;; Flycheck
(with-eval-after-load "flycheck"
  (define-key flycheck-mode-map (kbd "M-p") #'flycheck-previous-error)
  (define-key flycheck-mode-map (kbd "M-n") #'flycheck-next-error)
  (flycheck-pos-tip-mode))

;;; Flymake
(autoload #'flymake-mode "flymake" nil t)
(global-set-key (kbd "C-c C-f m") #'flymake-mode)
(setq flymake-no-changes-timeout 2)

(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "C-c k") 'flymake-show-diagnostics-buffer)
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;;; yasnippet
(setq yas-snippet-dirs
  ;; personal snippets
  '("~/.emacs.d/snippets"))

(autoload 'yas-minor-mode "yasnippet")
(add-hook 'prog-mode-hook 'yas-minor-mode)

(with-eval-after-load "yasnippet"
  (add-hook 'snippet-mode-hook 'electric-pair-local-mode)
  (setq yas-prompt-functions
        '(yas-ido-prompt yas-x-prompt yas-completing-prompt))
  (let ((inhibit-message t))
    (yas-reload-all))

  (define-key yas-keymap [escape] nil)
  (define-key yas-keymap [tab] nil)
  (define-key yas-keymap (kbd "S-<tab>") nil)
  (define-key yas-keymap (kbd "TAB") nil)
  (define-key yas-keymap [return] 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "RET") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "S-<return>") 'yas-prev-field))

;; (autoload #'corfu-mode "corfu" nil t)
(autoload #'company-mode "company" nil t)

(add-hook 'prog-mode-hook #'company-mode)
(add-hook 'conf-mode-hook #'company-mode)

;; company
(setq company-frontends '(company-pseudo-tooltip-frontend
                          company-preview-if-just-one-frontend
                          company-echo-metadata-frontend
                          company-tng-frontend)

      ;; self insert the first candidate
      company-begin-commands '(self-insert-command)
      company-idle-delay 1
      company-echo-delay 0
      company-dabbrev-ignore-invisible t
      company-dabbrev-downcase nil
      company-tooltip-idle-delay 0.5
      company-tooltip-limit 10
      ;; annos align to the right
      company-tooltip-align-annotations t
      ;; not allow tooltip width to decrease
      company-tooltip-width-grow-only t
      ;; delay in secs until tooltip shows
      company-dabbrev-downcase nil
      ;; cancel manually-triggered compl when prefix gets too short (<3)
      company-abort-manual-when-too-short t
      ;; allow free typing anywhere
      ;; see https://github.com/company-mode/company-mode/blob/99915c5d509fa0238e00bebb3d75d45dd1eaf5dc/company-tng.el#L65
      company-require-match nil
      ;; turn off company mode in dired
      company-global-modes '(not dired-mode dired-sidebar-mode)
      ;; disable format margin since I use no icons/imgs in compl
      company-format-margin-function nil)

(setq-default company-backends
              '(company-capf company-files company-dabbrev-code))

(setq-default company-search-filtering t)

(with-eval-after-load "company"
  (require 'company-tng)
  (require 'company-template)
  (add-hook 'company-mode-hook 'company-tng-mode)
  (define-key company-mode-map [tab] '+complete)
  (define-key company-mode-map (kbd "TAB") '+complete)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map [escape] nil)
  (define-key company-active-map [return] nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "SPC") nil)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-template-nav-map (kbd "RET") 'company-template-forward-field)
  (define-key company-template-nav-map [return] 'company-template-forward-field)
  (define-key company-template-nav-map (kbd "TAB") nil)
  (define-key company-template-nav-map [tab] nil))

(require 'vertico)
(vertico-mode)

;; marginalia
(require 'marginalia)
(marginalia-mode 1)

;; orderless (suggested by a friend)
;; for fuzzy search in minibuffer
(defun friend/use-orderless-in-minibuffer ()
  (setq-local completion-styles '(orderless)))
(add-hook 'minibuffer-setup-hook #'friend/use-orderless-in-minibuffer)

;; tree-sitter Emacs 29
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (require 'treesit)
  (add-to-list
   'treesit-language-source-alist
   '(clojure "https://github.com/sogaiu/tree-sitter-clojure.git")))

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        ;; (clojure-mode . clojure-ts-mode)
        ;; (rust-mode . rust-ts-mode)
        (typescript-mode . typescript-ts-mode)))

;; tree-sitter package for Emacs 28 compatibility
(require 'tree-sitter)
(require 'tree-sitter-langs)

(provide 'init-completion)
;;; init-completion.el ends here
