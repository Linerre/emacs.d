;;; early-init.el
;;; Commentary:

;;; Borrowed from Purcell's config
;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends
