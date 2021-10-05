;; -*- lexical-binding: t -*-
;; my functions for various purposes
;; Author: Errelin
;; Last Change: Thu Sep 30 20:06:59 2021

;; --------- VARIABLES --------
(defconst *is-win* (string-equal system-type "windows-nt"))
(defconst *is-mac* (string-equal system-type "darwin"))
(defconst *is-linux* (string-equal system-type "gnu/linux"))

;; --------- TIME ----------
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
