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
       (format "%s -o %s.out %s"
               (if  (equal (file-name-extension file) "cpp") "g++" "gcc")
               (file-name-sans-extension file)
               file)))
    (compile compile-command)))

(with-eval-after-load "cc-mode"
  (define-key c-mode-base-map [f9] #'+code-compile)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure))

(provide 'lang-cpp)
;; lang-cpp.el ends here
