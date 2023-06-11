;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(sup '(clojure-ts-mode :type git :host github :repo "clojure-emacs/clojure-ts-mode"))

;; (add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-ts-mode))

(autoload #'cider-mode "cider-mode" nil t)

(defun +cljs-company-backends ()
  (setq-local company-backends
              '(company-semantic
                company-keywords
                company-files
                company-dabbrev-code
                company-capf company-gtags)))

(setq clojure-indent-style 'always-indent
      clojure-align-forms-automatically t
      clojure-toplevel-inside-comment-form t)
(setq-local flycheck-checker 'clj-kondo-clj)

(add-to-list 'tree-sitter-major-mode-language-alist '(clojurescript-mode . clojure))

(add-function :before-until tree-sitter-hl-face-mapping-function
              (lambda (capture-name)
                (pcase capture-name
                  ("clj.ns" 'font-lock-function-name-face))))


(with-eval-after-load "clojure-mode"
  (require 'flycheck-clj-kondo)
  (add-hook 'clojure-mode-hook 'flycheck-mode)
  (eldoc-mode -1)             ; avoid overriding flycheck hints
  (put-clojure-indent 'or 0)
  (put-clojure-indent 'and 0)
  ;; (put-clojure-indent 'reg-sub 1)
  ;; (put-clojure-indent 'reg-event-db 1)
  ;; (put-clojure-indent 'reg-event-fx 1)
  ;; (put-clojure-indent 'reg-fx 1)
  ;; cider-jack-in will enable cider-mode so there's no need to hook it to
  ;; clojure-mode. But doing so will allow immediate access to cider-mode
  ;; after entering a clj buffer. REPL is not always necessary however.
  (define-key clojure-mode-map (kbd "C-c j") #'cider-jack-in)
  (define-key clojure-mode-map (kbd "C-c c") #'cider-connect)
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  (add-hook 'clojure-mode-hook #'direnv-mode)
  (add-hook 'clojure-mode-hook #'+hl-cmt-kws)
  ;; tree-sitter
  (add-hook 'clojure-mode-hook #'tree-sitter-mode)
  (add-hook 'clojure-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'clojure-mode-hook #'ts-fold-mode)
  ;; (add-hook 'clojurescript-mode-hook #'+cljs-company-backends)
  ;; (add-hook 'clojure-mode-hook
  ;;           (lambda ()
  ;;             (tree-sitter-hl-add-patterns nil
  ;;               [(list_lit (_) (sym_lit (sym_name)) @clj.ns)])))
  )


(setq nrepl-log-messages t
        nrepl-hide-special-buffers t
        cider-repl-result-prefix ";;=> "
        cider-connection-message-fn nil
        cider-repl-display-help-banner nil
        ;; cider-use-overlays nil
        ;; spill the errors to the error buffer instead of repl
        cider-show-error-buffer 'except-in-repl
        cider-print-fn 'zprint)

(with-eval-after-load "cider"
  ;; Make RET break a line and C-j to trigger eval
  (define-key cider-repl-mode-map (kbd "RET") #'cider-repl-newline-and-indent)
  (define-key cider-repl-mode-map (kbd "<return>") #'cider-repl-newline-and-indent)
  (define-key cider-repl-mode-map (kbd "M-<return>") #'cider-repl-return)
  (define-key cider-repl-mode-map (kbd "M-RET") #'cider-repl-return)
  (cider-add-to-alist 'cider-jack-in-dependencies
                      "zprint/zprint" "1.2.3"))

(provide 'lang-clojure)
;;; lang-clojure.el ends here
