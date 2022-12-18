;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(sup 'clojure-mode)
(sup 'cider)
(sup 'clj-refactor)
(sup 'flycheck)
(sup 'flycheck-clj-kondo)
(sup 'flycheck-joker)

;;; clojure-mode
(defun +cljs-company-backends ()
  (setq-local company-backends
              '(company-semantic
                company-keywords
                company-files
                company-dabbrev-code
                company-capf company-gtags)))

(setq clojure-toplevel-inside-comment-form t)

(autoload #'clojure-mode "clojure-mode" nil t)

(with-eval-after-load "clojure-mode"
  (require 'flycheck-clj-kondo)
  (add-hook 'clojure-mode-hook 'flycheck-mode)
  (eldoc-mode -1)             ; avoid overriding flycheck hints
  (setq clojure-indent-style 'always-indent
        clojure-align-forms-automatically t)
  (setq-local flycheck-checker 'clj-kondo-clj)
  (put-clojure-indent 'or 0)
  (put-clojure-indent 'and 0)
  ;; (put-clojure-indent 'reg-sub 1)
  ;; (put-clojure-indent 'reg-event-db 1)
  ;; (put-clojure-indent 'reg-event-fx 1)
  ;; (put-clojure-indent 'reg-fx 1)
  ;; (add-hook 'clojure-mode-hook 'paredit-mode)
  ;; cider-jack-in will enable cider-mode so there's no need to hook it to
  ;; clojure-mode. But doing so will allow immediate access to cider-mode
  ;; after entering a clj buffer. REPL is not always necessary however.
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (add-hook 'clojure-mode-hook 'direnv-mode)
  (add-hook 'clojurescript-mode-hook #'+cljs-company-backends)
  (define-key clojure-mode-map (kbd "C-c j") #'cider-jack-in)
  (define-key clojure-mode-map (kbd "C-c c") #'cider-connect))

;; cider-mode -- an Emacs minor mode
(with-eval-after-load "cider"
  (setq nrepl-log-messages t
        nrepl-hide-special-buffers t
        cider-repl-result-prefix ";; =>"
        cider-connection-message-fn nil
        cider-repl-display-help-banner nil
        ;; cider-use-overlays nil
        ;; spill the errors to the error buffer instead of repl
        cider-show-error-buffer 'except-in-repl
        cider-print-fn 'zprint)
  (cider-add-to-alist 'cider-jack-in-dependencies
                      "zprint/zprint" "1.2.3"))

(with-eval-after-load "cider-repl"
  ;; (add-hook 'cider-repl-mode-hook #'electric-pair-mode)
  )

(provide 'lang-clojure)
;;; lang-clojure.el ends here
