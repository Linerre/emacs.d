;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load "jsonrpc"
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil))

;;; eglot-booster
(autoload 'eglot "eglot" nil t)

;;; eglot
(setq eglot-ignored-server-capabilities
      '(:codeLensProvider
        :colorProvider
        :documentHighlightProvider
        :documentOnTypeFormattingProvider
        :documentRangeFormattingProvider
        :documentLinkProvider
        :inlayHintProvider))
(setq eglot-autoshutdown t)
(setq eglot-stay-out-of '(yasnippet))
(setq eglot-send-changes-idle-time 1.0)
(setq read-process-output-max (* 1024 1024)) ; 1MB

(defun eglot-setup-eldoc ()
  (setq-local eldoc-echo-area-use-multiline-p nil) ; always truncate
  (setq-local eldoc-documentation-functions
              '(eglot-hover-eldoc-function
                flymake-eldoc-function
                eglot-signature-eldoc-function)))

(add-hook 'eglot-managed-mode-hook #'eglot-setup-eldoc)
(setq eglot-booster-io-only t)
(setq eglot-booster-no-remote t)
(add-hook 'eglot-managed-mode-hook #'eglot-booster-mode)
(with-eval-after-load "eglot"
  (setq eglot-code-action-indications '(eldoc-hint))
  (setq eglot-events-buffer-config '(:size 0 :format full))
  (setq eglot-events-buffer-size 0)
  (define-key eglot-mode-map (kbd "<C-return>") 'eglot-code-actions)
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))

(setq-default eglot-workspace-configuration
              '(:rust-analyzer
                ( :hover (:memoryLayout (:enable :json-false))
                  :semanticHighlighting ( :operator (:enable :json-false)
                                          :doc (:comment (:inject (:enable :json-false)))
                                          :nonStandardTokens :json-false
                                          :strings (:enable :json-false))
                  :completion (:hideDeprecated t))))

;; (require 'init-lsp)

;;; Flycheck
(with-eval-after-load "flycheck"
  (flycheck-add-mode 'javascript-eslint 'jtsx-jsx-mode)
  (flycheck-add-mode 'javascript-eslint 'jtsx-tsx-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
  (define-key flycheck-mode-map (kbd "M-p") #'flycheck-previous-error)
  (define-key flycheck-mode-map (kbd "M-n") #'flycheck-next-error)
  (flycheck-pos-tip-mode))

;;; Flymake
(autoload #'flymake-mode "flymake" nil t)
(global-set-key (kbd "C-c C-f m") #'flymake-mode)
(setq flymake-no-changes-timeout 2)

(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "C-c l") #'flymake-show-diagnostics-buffer)
  (define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error))

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

;;; company
(autoload #'company-mode "company" nil t)
(setq-default company-backends
      '(company-capf
        company-files
        company-dabbrev-code
        company-keywords
        company-dabbrev))
(setq-default company-search-filtering t)

(setq company-idle-delay 0.5
      company-echo-delay 0
      company-dabbrev-ignore-invisible t
      company-dabbrev-downcase nil
      company-selection-wrap-around t
      ;; company-async-redisplay-delay 0.5
      ;; company-async-wait 0.5
      ;; company-show-quick-access nil
      company-tooltip-idle-delay 0.5
      company-tooltip-limit 10
      ;; annos align to the right
      company-tooltip-align-annotations t
      company-tooltip-maximum-width 60
      ;; not allow tooltip width to decrease
      company-tooltip-width-grow-only t
      ;; use text icon
      company-format-margin-function #'company-text-icons-margin)

(defun +complete ()
  "Expand snippet when there is one; otherwise, fall back on company."
  (interactive)
  (or (yas/expand)
      (company-indent-or-complete-common nil)))

(with-eval-after-load "company"
  (require 'company-tng)
  (add-hook 'company-mode-hook 'company-tng-mode)
  (keymap-set company-active-map "TAB" nil)
  (define-key company-active-map [tab] nil)
  (keymap-set company-mode-map "M-n" 'company-complete-common)
  ;; (keymap-set company-active-map "C-n" nil)
  ;; (keymap-set company-active-map "C-p" nil)
  ;; (keymap-set company-active-map "M-n" 'company-select-next)
  ;; (keymap-set company-active-map "M-p" 'company-select-previous)
  (define-key company-active-map [escape] nil)
  (define-key company-active-map [return] nil)
  (keymap-set company-active-map "RET" nil)
  (keymap-set company-active-map "SPC" nil))

(add-hook 'prog-mode-hook #'company-mode)
(add-hook 'conf-mode-hook #'company-mode)

(vertico-mode)
(with-eval-after-load "vertico"
  (setq vertico-count 6
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t))
;; (icomplete-mode t)

;; orderless (suggested by a friend)
;; for fuzzy search in minibuffer
(defun friend/use-orderless-in-minibuffer ()
  (setq-local completion-styles '(orderless flex basic)))
(add-hook 'minibuffer-setup-hook #'friend/use-orderless-in-minibuffer)

;; tree-sitter (Emacs >= 29)
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (require 'treesit)
  (add-to-list 'treesit-language-source-alist
               '(clojure "https://github.com/sogaiu/tree-sitter-clojure.git"))
  (add-to-list 'treesit-language-source-alist
               '(typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                            "v0.23.2"
                            "typescript/src"))
  (add-to-list 'treesit-language-source-alist
               '(tsx "https://github.com/tree-sitter/tree-sitter-typescript"
                            "v0.23.2"
                            "tsx/src"))
  (add-to-list 'treesit-language-source-alist
               '(javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                            "v0.23.2"
                            "src"))
  (add-to-list 'treesit-language-source-alist
               '(rust "https://github.com/tree-sitter/tree-sitter-rust"
                            "v0.23.2"
                            "src"))
  (add-to-list 'treesit-language-source-alist
               '(java "https://github.com/tree-sitter/tree-sitter-java"
                      "v0.23.5"
                      "src"))
  (add-to-list 'treesit-language-source-alist
               '(c "https://github.com/tree-sitter/tree-sitter-c"
                      "v0.24.1"
                      "src")))

;; (treesit-install-language-grammar 'javascript)
;; (treesit-install-language-grammar 'typescript)
;; (treesit-install-language-grammar 'tsx)
;; (treesit-install-language-grammar 'rust)
;; (treesit-install-language-grammar 'java)
;; (treesit-install-language-grammar 'c)

(setq major-mode-remap-alist
      '((typescript-mode . typescript-ts-mode)
        (c-mode . c-ts-mode)
        (java-mode . java-ts-mode)))

(provide 'init-completion)
;;; init-completion.el ends here
