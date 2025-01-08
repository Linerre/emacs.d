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
;; (custom-set-faces
;;  '(mode-line ((t :inherit variable-pitch)))
;;  '(mode-line-inactive ((t :inherit variable-pitch))))

;; Themes
(defvar my-themes
  '(
    ;; alabaster
    chacha
    carbon
    console
    gruber-darker
    ft
    paperlike))

;; themes
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
  (load-theme 'gruber-darker t nil))

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
                " "
                mode-line-misc-info
                " "
                mode-line-end-spaces))

;; sidebar
;; tree sidebar is useful when viewing a project
(sup 'dired-sidebar)
(setq dired-sidebar-theme 'ascii
      dired-sidebar-width 30
      dired-sidebar-use-custome-font t
      dired-sidebar-face '(:weight bold))

(autoload
  #'dired-sidebar-toggle-sidebar "dired-sidebar" nil t)
(global-set-key (kbd "<f8>") #'dired-sidebar-toggle-sidebar)
(with-eval-after-load "dired-sidebar"
  (add-hook 'dired-sidebar-mode-hook 'hl-line-mode))


(provide 'init-gui)

;;; init-gui.el ends here
