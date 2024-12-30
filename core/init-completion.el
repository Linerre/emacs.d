;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; eglot
(autoload 'eglot "eglot" nil t)
(autoload #'eglot-booster-mode "eglot-booster" nil t)

(setq eglot-booster-io-only t)
(setq eglot-ignored-server-capabilities
      '(:hoverProvider
        :documentHighlightProvider
        :inlayHintProvider
        :workspaceSymbolProvider))
(setq eglot-autoshutdown t)
(setq eglot-stay-out-of '(yasnippet))
(setq eglot-send-changes-idle-time 2.0)

(defvar jsonrpc-log-event-p nil)

(defun jsonrpc--log-event-advice (f &rest args)
  (if jsonrpc-log-event-p (apply f args)))

(advice-add #'jsonrpc--log-event :around #'jsonrpc--log-event-advice)

(defun +eglot-hook ()
  (setq eldoc-echo-area-use-multiline-p 1
        eldoc-echo-area-prefer-doc-buffer t)
  (setq eldoc-documentation-functions
        (cons #'flymake-eldoc-function
              (remove #'flymake-eldoc-function eldoc-documentation-functions))))

(with-eval-after-load "eglot"
  (add-hook 'eglot-managed-mode-hook #'+eglot-hook)
  (eglot-booster-mode)
  (define-key eglot-mode-map (kbd "C-c e d") #'eldoc-doc-buffer)
  (set-face-attribute 'eglot-highlight-symbol-face nil
                      :background "#B3D7FF")
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode) .
                 ("typescript-language-server" "--stdio"))))

;;; LSP
(autoload 'lsp "lsp-mode" nil t)

;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(setq lsp-keymap-prefix "C-c l"
      lsp-enable-symbol-highlighting nil
      lsp-enable-dap-auto-configure nil
      lsp-enable-snippt nil
      ;; lsp-modeline-diagnostics-enable nil
      lsp-lens-enable nil
      lsp-inlay-hint-enable nil
      lsp-headerline-breadcrumb-enable nil
      lsp-completion-show-detail nil
      lsp-completion-show-label-description nil
      ;; lsp-signature-auto-activate nil
      lsp-signature-render-documentation nil
      ;; this just turns off company as capf, see the folloiwng two threads
      ;; https://github.com/emacs-lsp/lsp-mode/issues/3215
      ;; https://github.com/minad/corfu/issues/71
      ;; https://www.reddit.com/r/emacs/comments/ql8cyp/corfu_orderless_and_lsp/
      lsp-completion-provider :none
      lsp-signature-doc-lines 3
      lsp-modeline-code-action-fallback-icon "CA")

(with-eval-after-load "lsp-mode"
  (add-to-list 'lsp-language-id-configuration '(move-mode . "move"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "move-analyzer")
    :activation-fn (lsp-activate-on "move")
    :priority -1
    :server-id 'move-analyzer)))

;;; lsp booster for lsp-mode
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

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

(autoload #'corfu-mode "corfu" nil t)
(autoload #'company-mode "company" nil t)

;;; corfu
(with-eval-after-load "corfu"
  (setq corfu-cycle t) ;; Enable cycling
  (setq corfu-auto t)  ;; Enable auto completion
  (setq corfu-auto-delay 1)
  (setq corfu-separator ?\s)            ;; Orderless field separator
  (setq corfu-quit-no-match 'separator) ;; Quit completion if no match found
  (setq corfu-preview-current nil) ;; Disable current candidate preview
  (setq corfu-preselect 'prompt)   ;; Preselect the prompt
  (setq corfu-on-exact-match 'insert) ;; Configure handling of exact matches
  (setq corfu-scroll-margin 5)        ;; Use scroll margin
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

(require 'vertico)
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

;; tree-sitter Emacs 29
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
                            "v0.23.1"
                            "src")))

;; (treesit-install-language-grammar 'javascript)
;; (treesit-install-language-grammar 'typescript)
;; (treesit-install-language-grammar 'tsx)

(setq major-mode-remap-alist
      '((typescript-mode . typescript-ts-mode)
        (c-mode . c-ts-mode)))

(provide 'init-completion)
;;; init-completion.el ends here
