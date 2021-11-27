(straight-use-package
 '(exec-path-from-shell
   :type git :host github :repo "purcell/exec-path-from-shell"))
 (when (and (memq window-system '(ns)) (display-graphic-p))
   (exec-path-from-shell-initialize))
