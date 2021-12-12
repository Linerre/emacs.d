
;;; -*- lexical-binding: t -*-
;;; ------------------- COMPLETION  -----------------------
;; use snippet when there is one, otherwise, company

(straight-use-package 'company)
(straight-use-package 'yasnippet)
(straight-use-package 'selectrum)



(defun +complete ()
  (interactive)
  (or (yas/expand)
      (company-indent-or-complete-common nil)))

;;; yasnippet
(setq yas-snippet-dirs
  ;; personal snippets
  '("~/.emacs.d/snippets"))

(setq yas-prompt-functions '(yas-ido-prompt yas-x-prompt yas-completing-prompt))

(autoload 'yas-minor-mode "yasnippet")
(add-hook 'prog-mode-hook 'yas-minor-mode)

(with-eval-after-load "yasnippet"
  (let ((inhibit-message t))
    (yas-reload-all))

  (define-key yas-keymap [escape] nil)
  (define-key yas-keymap [tab] nil)
  (define-key yas-keymap (kbd "S-<tab>") nil)
  (define-key yas-keymap (kbd "TAB") nil)
  (define-key yas-keymap [return] 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "RET") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "S-<return>") 'yas-prev-field))

;; company

(setq
 company-frontends '(company-pseudo-tooltip-frontend
                     company-preview-if-just-one-frontend
                     company-echo-metadata-frontend
                     company-tng-frontend)

      ;; self insert the first candidate
      company-begin-commands '(self-insert-command)
      company-idle-delay 0.1
      ;; annos align to the right
      company-tooltip-align-annotations t
      ;; not allow tooltip width to decrease
      company-tooltip-width-grow-only t
      ;; delay in secs until tooltip shows
      company-tooltip-idle-delay 0.4
      company-dabbrev-downcase nil
      ;; cancel manually-triggered compl when prefix gets too short (<3)
      company-abort-manual-when-too-short t
      ;; allow free typing anywhere
      ;; see https://github.com/company-mode/company-mode/blob/99915c5d509fa0238e00bebb3d75d45dd1eaf5dc/company-tng.el#L65
      company-require-match nil
      ;; turn off company mode in dired
      company-global-modes '(not dired-mode dired-sidebar-mode)
      ;; disable format margin since I use no icons/imgs in compl
      company-format-margin-function nil)

(autoload #'company-mode "company")

(dolist (hook '(prog-mode-hook conf-mode-hook))
  (add-hook hook 'company-mode)
  (add-hook hook 'company-mode))

(with-eval-after-load "company"
  (require 'company-tng)
  (require 'company-template)
  (add-hook 'company-mode-hook 'company-tng-mode)

  (define-key company-mode-map [tab] '+complete)
  (define-key company-mode-map (kbd "TAB") '+complete)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map [escape] nil)
  (define-key company-active-map [return] nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "SPC") nil)
  (define-key company-active-map (kbd "SPC") nil)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next)

  (define-key company-template-nav-map (kbd "RET") 'company-template-forward-field)
  (define-key company-template-nav-map [return] 'company-template-forward-field)
  (define-key company-template-nav-map (kbd "TAB") nil)
  (define-key company-template-nav-map [tab] nil))


(require 'selectrum)
(selectrum-mode t)

(provide 'init-completion)
;;; init-completion ends here
