;; -*- lexical-binding: t -*-
;; start emacs server ONLY when:
;; 1) GUI is running
;; 2) no server is running ATM
;; Inspired by purcell and dogEmacs
;; Last Change: Mon Aug 23 21:51:35 2021

;;; Allow access from emacsclien
(require 'server)
(add-hook 'after-init-hook
          (lambda ()
            (if (display-graphic-p)
              (unless (server-running-p)
                (server-start)))))

(provide 'init-server)
;;; init-server ends here
