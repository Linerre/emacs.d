;;; -*- lexical-binding: t -*-
;;; Commentary: config for LSP mode
;;; Code:

;;; LSP
(autoload 'lsp "lsp-mode" nil t)
;;; for lsp-mode to use plist
(setenv "LSP_USE_PLISTS" "true")

;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(setq lsp-keymap-prefix "C-c l"
      lsp-enable-symbol-highlighting nil
      lsp-enable-dap-auto-configure nil
      lsp-enable-snippt nil
      ;; lsp-modeline-diagnostics-enable nil
      lsp-use-plists t
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

;; lsp booster for lsp-mode
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

(provide 'init-lsp)
