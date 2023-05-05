;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Mode line
(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '(" %l,%c")) ; Emacs 28
;; (setq mode-line-defining-kbd-macro
;;       (propertize " Macro" 'face 'mode-line-emphasis))

(setq mode-line-compact nil)            ; Emacs 28
(setq-default mode-line-format
              '("%Z"
                "%*"
                " "
                ;; mode-line-frame-identification
                ;; (:eval
                ;;  (+project-indicator buffer-file-name))
                mode-line-buffer-identification
                " "
                mode-line-position
                (vc-mode vc-mode)
                " "
                mode-name
                mode-line-misc-info
                mode-line-end-spaces))

(provide 'init-modeline)

;;; init-modeline.el ends here
