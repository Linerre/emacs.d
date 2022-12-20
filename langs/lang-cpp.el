;; -*- lexical-binding: t; -*-
;; Commentary:
;; Config for c and cpp

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
  (setq-local c-basic-offset 8
              tab-width 8
              indent-tabs-mode t)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'inline-open '+)
  (c-set-offset 'block-open '+)
  (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
  (c-set-offset 'case-label '+))

(with-eval-after-load "cc-mode"
  (define-key c-mode-base-map [f9] #'+code-compile)
  (add-hook 'c-mode-hook #'+my-c-indent)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure))

(provide 'lang-cpp)
;; lang-cpp.el ends here
