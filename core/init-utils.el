;; -*- lexical-binding: t -*-;
;; Commentary:
;; Interactive commands and con defined here for all modes
;; For fns and consts to use in other modules, refer to init-macros.el
;; Code:

;; which-key
(straight-use-package 'which-key)
(require 'which-key)
(which-key-mode)

;;  VARIABLES
(defconst *is-win* (string-equal system-type "windows-nt"))
(defconst *is-mac* (string-equal system-type "darwin"))
(defconst *is-linux* (string-equal system-type "gnu/linux"))

;; TIME FUNCS
(defun +insert-timestamp ()
  "Insert timestamp of the current point in time."
  (interactive)
  (insert (current-time-string)))

(defun +insert-today-date-string ()
  "Insert today's date in the format of YYYY-MM-DD WEK."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a" (current-time))))

(defun +insert-tomorrow-date-string ()
  "Insert tomorrow's date in the format of YYYY-MM-DD WEK."
  (interactive)
  (setq tmw (+ (* 24 60 60) (time-convert nil 'integer)))
  (insert (format-time-string "%Y-%m-%d %a" tmw)))


(global-set-key (kbd "C-c t s") #'+insert-timestamp)
(global-set-key (kbd "C-c t d") #'+insert-today-date-string)
(global-set-key (kbd "C-c t m") #'+insert-tomorrow-date-string)

(provide 'init-utils)
;; init-utils.el ends here
