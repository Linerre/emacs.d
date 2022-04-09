;;; -*- lexical-binding: t; -*-
;;; Commentary
;;; Code:

(sup
 '(emacs-sql-indent :type git :host github :repo "alex-hhh/emacs-sql-indent"))
(sup
 '(sqlup-mode :type git :host github :repo "Trevoke/sqlup-mode.el"))

(with-eval-after-load "sql"
  (add-hook 'sql-mode-hook 'sqlind-minor-mode)
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (define-key sql-mode-map (kbd "C-c u") 'sqlup-capitalize-keywords-in-region))

(provide 'lang-sql)
;;; lang-sql.el ends here
