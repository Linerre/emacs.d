;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Configurations for lisps (elisp, clojure, scheme, etc)
;;; Code:

;; elisp
(autoload #'highlight-defined-mode "highlight-defined" nil t)

(with-eval-after-load "elisp-mode"
  (add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode)
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (add-hook 'after-save-hook #'check-parens)))
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode))

;; clojure
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
  (add-hook 'clojure-mode-hook #'+hl-cmt-kws))

(setq nrepl-log-messages t
        nrepl-hide-special-buffers t
        cider-repl-result-prefix ";;=> "
        cider-connection-message-fn nil
        cider-repl-display-help-banner nil
        ;; cider-use-overlays nil
        ;; spill the errors to the error buffer instead of repl
        cider-show-error-buffer 'except-in-repl)

(with-eval-after-load "cider"
  ;; Make RET break a line and C-j to trigger eval
  (define-key cider-repl-mode-map (kbd "RET") #'cider-repl-newline-and-indent)
  (define-key cider-repl-mode-map (kbd "<return>") #'cider-repl-newline-and-indent)
  (define-key cider-repl-mode-map (kbd "M-<return>") #'cider-repl-return)
  (define-key cider-repl-mode-map (kbd "M-RET") #'cider-repl-return))

;; scheme
;; (setq scheme-program-name "mit-scheme")
;; (setq scheme-program-name "csi -:c")
(setq scheme-program-name "guile")
(with-eval-after-load "scheme-mode"
  (add-hook 'scheme-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'geiser-mode)
  (define-key scheme-mode-map (kbd "C-c g") #'geiser))

(with-eval-after-load "geiser-repl"
  (add-hook 'geiser-repl-mode-hook #'paredit-mode))


(provide 'lang-lisps)
;;; lang-lisps.el ends here
