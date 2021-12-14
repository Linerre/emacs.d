;; Mode Line  -*- lexical-binding: t; -*-



(defun +format-mode-line ()
  (let* ((lhs '(;(:eval (meow-indicator)) no longer necessary; handled by meow
                "[%2l:%2c]"
                " "
                "%*:%I"
                ;;(vc-mode vc-mode)
                (:eval (+vc-branch-name))
                (:eval (when (bound-and-true-p flycheck-mode) flycheck-mode-line))
                (:eval (when (bound-and-true-p flymake-mode) flymake-mode-line-format))))
         (rhs '("%b"
                " "
                (:eval mode-name)
                ))
         ;;(ww (window-width))
         (lhs-str (format-mode-line lhs))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    (format "%s%s%s"
            lhs-str
            (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,rhs-w)))))
            rhs-str)))

(setq-default mode-line-format '((:eval (+format-mode-line))))
(setq-default header-line-format nil)

(provide 'init-modeline-dog)
