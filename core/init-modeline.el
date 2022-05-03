;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Below modeline is mostly based on Protesilaos' Emacs config
;;; <https://protesilaos.com/emacs/dotemacs> -- 6.2 Modeline section
;;; Code:
;;; Mode line
(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '(" %l,%c")) ; Emacs 28
;; (setq mode-line-defining-kbd-macro
;;       (propertize " Macro" 'face 'mode-line-emphasis))

;; Thanks to Daniel Mendler for this!  It removes the square brackets
;; that denote recursive edits in the modeline.  I do not need them
;; because I am using Daniel's `recursion-indicator':
;; <https://github.com/minad/recursion-indicator>.
(setq-default mode-line-modes
              (seq-filter (lambda (s)
                            (not (and (stringp s)
                                      (string-match-p
                                       "^\\(%\\[\\|%\\]\\)$" s))))
                          mode-line-modes))

(setq mode-line-compact nil)            ; Emacs 28
(setq-default mode-line-format
              '(mode-line-front-space
                "%Z"
                ;; mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                (:eval (file-relative-name buffer-file-name projects-root))
                ;; mode-line-buffer-identification
                " "
                mode-line-position
                ;; mode-line-modes
                (vc-mode vc-mode)
                " "
                mode-name
                mode-line-misc-info
                mode-line-end-spaces))
(provide 'init-modeline)

;;; init-modeline.el ends here
