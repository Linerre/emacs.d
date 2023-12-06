;;; -*- lexical-binding: t; -*-

;;; Commentary:
;;; Shell-script-mode and shell-mode config

;;; Code:
(defun insert-bash-shebang ()
  "Inserts the shebang line for Bash at the beginning of the script."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "#!/usr/bin/env bash\n")))

;; a child mode of shell-script-mode
(autoload #'ebuild-mode "ebuild-mode" nil t)

;; shell-script-mode
(add-to-list 'auto-mode-alist '("bash_profile\\'\\|[bz]a?shrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("PKGBUILD\\'" . sh-mode))

;; sh-script.el
(with-eval-after-load "sh-script"
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
  (add-hook 'sh-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c #") #'insert-bash-shebang)))
  )

;; vars can be set:
;; sh-basic-offset -- defaults to 4
;; sh-indentation -- defaults to 4 -- how many spaces to put in a deeper level
;; sh-indent-for-case-label
;; sh-indent-for-case-alt

(provide 'lang-shell)
