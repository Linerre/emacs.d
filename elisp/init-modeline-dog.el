;; Mode Line  -*- lexical-binding: t; -*-

(setq-default header-line-format nil)

(defun +format-mode-line ()
  (let* ((lhs '((:eval (meow-indicator))
                "[%2l:%2c]"
                " "
                "("
                (-3 "%p")
                "/%I)"
                (vc-mode vc-mode)
                (:eval (when (bound-and-true-p flycheck-mode) flycheck-mode-line))
                (:eval (when (bound-and-true-p flymake-mode)
                         flymake-mode-line-format))))
         (rhs '("%b"
                " ⊆ "
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

(provide 'init-modeline-dog)