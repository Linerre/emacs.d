;;; -*- lexical-binding: t; -*-

;;; Commentary:
;;; Config for tools built in Emacs. Many
;;; settings take effect only in GUI.

;;; Code:

;; eww
(add-hook 'eww-mode-hook
          (lambda ()
            (setq left-fringe-width 2
                  right-fringe-width 2)))


(provide 'init-etools)
