;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(sup 'company)
(sup 'yasnippet)
(sup 'consult)
(sup 'marginalia)
;; (sup 'selectrum)
(sup 'vertico)
(sup 'corfu)
(sup 'cape)
;; (sup
;;  '(corfu-terminal
;;    :type git
;;    :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
(sup
 '(cmt :type git :host gitlab :repo "protesilaos/mct"))

(defun +complete ()
  "Expand snippet when there is one; otherwise, fall back on company."
  (interactive)
  (or (yas/expand)
      (company-indent-or-complete-common nil)))

;;; yasnippet
(setq yas-snippet-dirs
  ;; personal snippets
  '("~/.emacs.d/snippets"))

(autoload 'yas-minor-mode "yasnippet")
(add-hook 'prog-mode-hook 'yas-minor-mode)

(with-eval-after-load "yasnippet"
  (add-hook 'snippet-mode-hook 'electric-pair-mode)
  (setq yas-prompt-functions
        '(yas-ido-prompt yas-x-prompt yas-completing-prompt))
  (let ((inhibit-message t))
    (yas-reload-all))

  (define-key yas-keymap [escape] nil)
  (define-key yas-keymap [tab] nil)
  (define-key yas-keymap (kbd "S-<tab>") nil)
  (define-key yas-keymap (kbd "TAB") nil)
  (define-key yas-keymap [return] 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "RET") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "S-<return>") 'yas-prev-field))

;; (autoload #'corfu-mode "corfu" nil t)
(autoload #'company-mode "company" nil t)

(if (display-graphic-p)
    (global-corfu-mode)
  (progn
    (add-hook 'prog-mode-hook #'company-mode)
    (add-hook 'conf-mode-hook #'company-mode)))

;; corfu
(with-eval-after-load "corfu"
  (setq completion-cycle-threshold 3
        tab-always-indent 'complete
        corfu-preview-current nil  ;; Preview currently selected candidate.
        corfu-preselect-first nil  ;; Disable candidate preselection
        )
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)

  ;; Add the following to proper modes, e.g. cape-ispell for plain text files
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)

  ;; keybindings here are not necessary. Just use TAB
  ;; (global-set-key (kbd "C-c x f") #'cape-file)
  ;; (global-set-key (kbd "C-c c d") #'cape-dabbrev)
  ;; (global-set-key (kbd "C-c c i") #'cape-ispell)
  )

;; company
(setq
 company-frontends '(company-pseudo-tooltip-frontend
                     company-preview-if-just-one-frontend
                     company-echo-metadata-frontend
                     company-tng-frontend)

      ;; self insert the first candidate
      company-begin-commands '(self-insert-command)
      ;; company-idle-delay 0.5
      company-dabbrev-ignore-invisible t
      company-dabbrev-downcase nil
      ;; annos align to the right
      company-tooltip-align-annotations t
      ;; not allow tooltip width to decrease
      company-tooltip-width-grow-only t
      ;; delay in secs until tooltip shows
      company-tooltip-idle-delay 0.5
      ;; cancel manually-triggered compl when prefix gets too short (<3)
      company-abort-manual-when-too-short t
      ;; allow free typing anywhere
      ;; see https://github.com/company-mode/company-mode/blob/99915c5d509fa0238e00bebb3d75d45dd1eaf5dc/company-tng.el#L65
      company-require-match nil
      ;; turn off company mode in dired
      company-global-modes '(not dired-mode dired-sidebar-mode)
      ;; disable format margin since I use no icons/imgs in compl
      company-format-margin-function nil)

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
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-template-nav-map (kbd "RET") 'company-template-forward-field)
  (define-key company-template-nav-map [return] 'company-template-forward-field)
  (define-key company-template-nav-map (kbd "TAB") nil)
  (define-key company-template-nav-map [tab] nil))



;; (require 'selectrum)
;; (selectrum-mode t)
;; (setq completion-styles
;;       '(basic substring initials flex partial-completion))
;; (setq completion-category-overrides
;;       '((file (styles . (basic partial-completion)))))

(require 'mct)
(setq mct-remove-shadowed-file-names t
      mct-hide-completion-mode-line t
      mct-show-completion-line-numbers nil
      mct-apply-completion-stripes t
      mct-minimum-input 3
      mct-live-update-delay 0.6
      mct-completions-format 'one-column)
 ; works when `file-name-shadow-mode' is enabled
(mct-mode 1)

;; marginalia
(require 'marginalia)
(marginalia-mode 1)

;; orderless (suggested by a friend)
;; for fuzzy search in minibuffer
(sup 'orderless)
(defun friend/use-orderless-in-minibuffer ()
  (setq-local completion-styles '(orderless)))
(add-hook 'minibuffer-setup-hook #'friend/use-orderless-in-minibuffer)

(provide 'init-completion)
;;; init-completion ends here
