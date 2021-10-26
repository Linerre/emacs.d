;;; -*- lexical-binding: t -*-

(setq package-enable-at-startup nil)
(setq comp-deferred-compilation-deny-list ())
(setq straight-vc-git-default-clone-depth 1)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package
 '(exec-path-from-shell
   :type git :host github :repo "purcell/exec-path-from-shell"))
 (when (memq window-system '(pc ns x))
   (exec-path-from-shell-initialize))

(provide 'init-straight)
