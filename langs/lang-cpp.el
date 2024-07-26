;; -*- lexical-binding: t; -*-
;; Commentary:
;; Config for c-like languages such as c and cpp (and java)

;; Code:
;; Inspired by 'compile-command doc
(defun +code-compile ()
  (interactive)
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
     (let ((file (file-name-nondirectory buffer-file-name)))
       ;; latest gcc defaults to -std=gnu17 (c17 + gnu extensions)
       (format "%s -Wall -o %s.out %s"
               (if  (equal (file-name-extension file) "cpp") "g++" "gcc")
               (file-name-sans-extension file)
               file)))
    (compile compile-command)))

(defun +my-c-indent ()
  ;; Set any indentation that needs a change
  (setq-local c-tab-always-indent t
              display-buffer-alist
              '(("\\*compilation\\*" (display-buffer-in-side-window))))
  (setq-local c-basic-offset 4
              tab-width 4
              indent-tabs-mode t)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'inline-open '+)
  (c-set-offset 'block-open '+)
  (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
  (c-set-offset 'case-label '+))

(setq ccls-executable "/usr/bin/ccls")
(add-hook 'c++-mode-hook (lambda ()
                           (modify-syntax-entry ?> "." c++-mode-syntax-table)
                           (modify-syntax-entry ?< "." c++-mode-syntax-table)
                           (setq-local c-basic-offset 2
                                       tab-width 2
                                       indent-tabs-mode t)
                           (require 'ccls)))

(add-hook 'java-mode-hook (lambda ()
                            ;; (setq c-default-style "java")
                            (setq-local c-basic-offset 2)
                            (setq-local tab-width 2)
                            (c-set-offset 'arglist-intro '+)
                            (c-set-offset 'arglist-close '0)
                            (c-set-offset 'case-label '+)))

;; there is no cc-mode entry but c-mode
(with-eval-after-load "cc-mode"
  (define-key c-mode-base-map [f9] #'+code-compile)
  (add-hook 'c-mode-hook #'+my-c-indent)
  (add-hook 'c-mode-hook #'flycheck-mode))

(provide 'lang-cpp)

;;; lang-cpp.el ends here
