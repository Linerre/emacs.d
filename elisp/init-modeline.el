;; Mode line -*- lexical-binding: t; -*-

;; Useful resources for making your own modeline:
;; 1. Elisp Manual:
;;   - https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Data.html
;;   - https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Top.html
;;   - https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Variables.html
;;   - https://www.gnu.org/software/emacs/manual/html_node/elisp/_0025_002dConstructs.html
;;   - https://www.gnu.org/software/emacs/manual/html_node/elisp/Properties-in-Mode.html
;; 2. Elsa:
;;   - https://www.gonsie.com/blorg/modeline.html
;;   - https://github.com/gonsie/dotfiles/blob/fff8749a9463b7e181bea74bf93a62ff27d64d2c/emacs/theme.el#L75
;; 3. A more fancy one:
;;   - https://kitchingroup.cheme.cmu.edu/blog/2014/09/19/A-git-status-Emacs-modeline/
;; 4. Clean mode line:
;;   - https://www.masteringemacs.org/article/hiding-replacing-modeline-strings

;; My modeline as below:
;; buffer-file-name [Row:Col] (Relative Position) Git-branch Flymake Modes-alist
(setq-default mode-line-format
              '((:eval (meow-indicator))
                "[%2l:%2c]"
                " "
                "("
                ;; truncate relative position to three chars
                (-3 "%p")
                "/%I)"
                ;; version control info
                (vc-mode vc-mode)
                " "
                ;; flycheck info
                (:eval (when (bound-and-true-p flycheck-mode) flycheck-mode-line))
                (:eval (when (bound-and-true-p flymake-mode)
                         flymake-mode-line-format))
                ;; fill spaces until right
                ;; :eval must be here in my case
                (:eval (propertize
                 " "
                 'display
                 `((space :align-to (- (+ right right-fringe right-margin)
                                       (+ 2
                                          ,(string-width mode-name)
                                          ,(string-width (buffer-name)))
                                          ;,(string-width mode-line-buffer-identification))
                                       )))))
                ;; show buffer-name, mode-name at the rightmost
                "%b"
                " "
                (:eval mode-name)
                ))

(provide 'init-modeline)
