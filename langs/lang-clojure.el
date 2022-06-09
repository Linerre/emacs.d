;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(sup 'clojure-mode)
(sup 'cider)
(sup 'clj-refactor)
(sup 'flycheck)
(sup 'flycheck-clj-kondo)
(sup 'flycheck-joker)
(sup 'paredit)

;;; clojure-mode
(setq clojure-toplevel-inside-comment-form t)

(autoload #'clojure-mode "clojure-mode" nil t)

(with-eval-after-load "clojure-mode"
  (setq cider-default-cljs-repl 'shadow) ;; prefered to be set in .dir-local.el
  (add-hook 'clojure-mode-hook 'paredit-mode)
  ;; cider-jack-in will enable cider-mode so there's no need to hook it to
  ;; clojure-mode. But doing so will allow immediate access to cider-mode
  ;; after entering a clj buffer. REPL is not always necessary however.
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (define-key clojure-mode-map (kbd "C-c j") #'cider-jack-in)
  (define-key clojure-mode-map (kbd "C-c c") #'cider-connect))

;; cider-mode -- an Emacs minor mode
(with-eval-after-load "cider"
  (setq nrepl-log-messages t
        nrepl-hide-special-buffers t
        ;; spill the errors to the error buffer instead of repl
        cider-show-error-buffer 'except-in-repl
        cider-print-fn 'zprint)
  (cider-add-to-alist 'cider-jack-in-dependencies
                      "zprint/zprint" "1.2.3"))

(with-eval-after-load "cider-repl"
    (add-hook 'cider-repl-mode-hook 'electric-pair-mode)
    ;; Tab doesn't complete instead it moves cursor as C-a was pressed
    ;; Use the default C-a/w/d to move to left/right/up windows
    ;; (add-hook 'cider-repl-mode-hook (lambda () (meow-mode -1)))
)

(provide 'lang-clojure)
;;; lang-clojure.el ends here
