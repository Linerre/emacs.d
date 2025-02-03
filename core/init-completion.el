;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; eglot-booster
(autoload 'eglot "eglot" nil t)

;;; eglot
(setq eglot-ignored-server-capabilities
      '(:codeLensProvider
        :documentHighlightProvider
        :hoverProvider
        :inlayHintProvider
        :workspaceSymbolProvider))
(setq eglot-autoshutdown t)
(setq eglot-stay-out-of '(yasnippet))
(setq eglot-send-changes-idle-time 1.0)
(defun eglot-setup-eldoc ()
  (setq-local eldoc-documentation-functions
              '(flymake-eldoc-function
                eglot-signature-eldoc-function
                eglot-hover-eldoc-function)))

(add-hook 'eglot-mode-hook #'eglot-setup-eldoc)

(with-eval-after-load "jsonrpc"
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil))

(with-eval-after-load "eglot"
  (setq eglot-events-buffer-config '(:size 0 :format full))
  (setq eglot-events-buffer-size 0))

;; (setq-default eglot-workspace-configuration
;;               '(:rust-analyzer (:hover (:memoryLayout (:enable :json-false))
;;                                 :typing (:excludeChars "([{"))))

(setq eglot-booster-io-only t)
(add-hook 'eglot-mode-hook #'eglot-booster-mode)

;; Or use package-vc-install
;; (when (executable-find "emacs-lsp-booster")
;;   (unless (package-installed-p 'eglot-booster)
;;     (and (fboundp #'package-vc-install)
;;          (package-vc-install '(eglot-booster :vc-backend Git :url
;;                     "https://github.com/jdtsmith/eglot-booster")))
;;     (setq eglot-booster-io-only t)
;;     (add-hook 'eglot-mode-hook #'elogt-booster-mode)))


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

(autoload #'corfu-mode "corfu" nil t)
(autoload #'company-mode "company" nil t)

;;; corfu
(with-eval-after-load "corfu"
  (setq corfu-cycle t)                  ; Enable cycling
  (setq corfu-auto t)                   ; Enable auto completion
  (setq corfu-auto-delay 1)
  (setq corfu-preview-current nil) ; Disable current candidate preview
  (setq corfu-preselect 'prompt)   ; Preselect the prompt
  (setq corfu-on-exact-match 'insert) ; Configure handling of exact matches
  (setq corfu-scroll-margin 5)        ; Use scroll margin
  (setq corfu-max-width 60)           ; make width for popup
  (setq tab-always-indent 'complete)
  (setq corfu-quit-at-boundary 'separator)
  (setq-local completion-styles '(basic)))

;;; cape
(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-file)
(add-hook 'completion-at-point-functions #'cape-elisp-block)

;;; company
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

(defun +complete ()
  "Expand snippet when there is one; otherwise, fall back on company."
  (interactive)
  (or (yas/expand)
      (company-indent-or-complete-common nil)))

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

;;; company for TUI and corfu for GUI
(if (display-graphic-p)
    (progn
      (add-hook 'prog-mode-hook #'corfu-mode)
      (add-hook 'conf-mode-hook #'corfu-mode))
  (progn
    (add-hook 'prog-mode-hook #'company-mode)
    (add-hook 'conf-mode-hook #'company-mode)))

(vertico-mode)
(with-eval-after-load "vertico"
  (setq vertico-count 5
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
                            "src")))

;; (treesit-install-language-grammar 'javascript)
;; (treesit-install-language-grammar 'typescript)
;; (treesit-install-language-grammar 'tsx)
;; (treesit-install-language-grammar 'rust)

(setq major-mode-remap-alist
      '((typescript-mode . typescript-ts-mode)
        (c-mode . c-ts-mode)))

(provide 'init-completion)
;;; init-completion.el ends here
