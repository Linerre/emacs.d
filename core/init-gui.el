;;; -*- lexical-binding: t -*-
;;; Commentary: color schemes, fonts, frames and modeline
;;; Code:

;;; Fonts
(defvar +chinese-font-family "LXGW Wenkai")
(defvar bitmap-font "Px437 IBM VGA 8x16-16")
(defvar sans-font "DejaVu Sans-14")
(defvar serif-font "DejaVu Serif-14")
(defvar mono-dejavu "DejaVu Sans Mono-14")
(defvar mono-monego "Monego-14")
(defvar mono-liberation "Liberation Mono-14")
(defvar mono-inconsolata "Inconsolata Medium-16:style=Regular")
(defvar mono-consolas "Consolas-14")
(defvar mono-mono "Monospace-13")

;;; frame: monospace font; modeline: sans font
(set-fontset-font t 'han (font-spec :family +chinese-font-family))
(set-face-attribute 'default nil :font mono-dejavu)
(set-face-attribute 'variable-pitch nil :family "Sans" :font sans-font)
(set-face-attribute 'fixed-pitch nil :font mono-dejavu)

;; Themes
(defvar my-themes
  '(chacha
    rustdoc-dark
    carbon
    ft
    ;; alabaster
    ;; gruber-darker
    ;; paperlike
    ;; console
    ))

(defun +toggle-themes ()
  "Load the new-theme and disable the current one."
  (interactive)
  (setq my-themes (append (cdr my-themes) (list (car my-themes))))
  (mapc #'disable-theme custom-enabled-themes)
  (let ((theme (car my-themes)))
    (load-theme theme t nil)))

(global-set-key (kbd "C-c m") #'+toggle-themes)

(if (display-graphic-p)
    (load-theme 'chacha t nil)
  (load-theme 'rustdoc-dark t nil))

;;; Mode line (Emacs > 28)
(setq mode-line-position-column-line-format '(" %l,%c"))
(setq mode-line-compact t)
(setq-default mode-line-format
              '("%Z"
                "%*"
                " "
                mode-line-frame-identification
                mode-line-buffer-identification
                " "
                mode-line-position
                (vc-mode vc-mode)
                " "
                mode-name
                " "
                (:eval (when envrc-mode envrc-lighter))
                " "
                (:eval (when flymake-mode flymake-mode-line-format))
                " "
                mode-line-misc-info
                " "
                mode-line-end-spaces))

;; tab bar
(setq tab-bar-new-button-show nil)
;; (setq tab-bar-close-button-show nil)

(provide 'init-gui)

;;; init-gui.el ends here
