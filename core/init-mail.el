;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configs for Email in Emacs
;;; sendmail is an soft link pointing to mail-sending package
;;; /usr/sbin/sendmail -> /usr/bin/msmtp
;;; Code:

;; TODO: gnus/notmuch

;; Compose and send email
(setq user-mail-address "errelinaaron@gmail.com"
      user-full-name "Noel Errenil"
      message-signature user-full-name
      message-send-mail-function #'message-use-send-mail-function
      send-mail-function #'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'ssl)

;; The following vars are for Emacs SMTP Library:
;; https://www.gnu.org/software/emacs/manual/html_node/smtpmail/index.html
;; With the exactly same msmtp config, 7 of 10 I couldn't send out the email.
;; My guess is that gmail doesn't support smtp well or msmtp is a bit old
;; (setq
;; send-mail-function 'smtpmail-send-it
;; message-send-mail-function 'smtpmail-send-it
;; )

(provide 'init-mail)
;;; init-mail.el ends here
