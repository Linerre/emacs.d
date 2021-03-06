;;; Various emacs built-in options set for both GUI and TUI
;;; Author: Errenil
;;; Last Change: 2021-11-18 Thu

(setq-default
 ;; startup config
 inhibit-startup-message t
 inhibit-startup-screen t
 inhibit-splash-screen t
 inhibit-x-resources t
 inhibit-startup-echo-area-message t
 initial-scratch-message ""
 ;; frame, window, buffer config
 ;; prefer horizental split
 split-height-threshold nil
 split-width-threshold 100
 ;; don't create lockfiles
 ;;create-lockfiles nil
 frame-resize-pixelwise t

 ;; UTF-8
 ;; There are also a couple of functions used to
 ;; set UTF-8 encoding, such as:
 ;; (prefer-coding-system 'utf-8)
 ;; (set-default-coding-system 'utf-8)
 ;; but that will interrupt this setq-default way
 ;; For more, see https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs
 buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system 'utf-8-unix
 default-keyboard-coding-system 'utf-8-unix
 default-process-coding-system '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system 'utf-8-unix
 default-terminal-coding-system 'utf-8-unix

 ;; vc-options
 vc-follow-symlinks t
 ;; no auto-saving and backups for now
 ;; for backups, look up these options:
 ;; backup-directory-alist
 ;; backup-by-copying t
 ;; delete-old-versions t
 ;; kept-new-versions 6
 ;; kept-old-versions 2
 ;; version-control t
 make-backup-files nil
 auto-save-default nil
 ;; mouse yank at point instead of click position.
 mouse-yank-at-point t
 ;; this fix the cursor movement lag
 auto-window-vscroll nil
 ;; The nano style for truncated long lines.
 auto-hscroll-mode 'current-line
 ;; scrolling
 scroll-conservatively 200
 ;; use thiner than default(6) window divider
 window-divider-default-right-width 1
 window-divider-default-bottom-width 1
 echo-keystrokes 0.01
 ;; no overline margin
 overline-margin 0
 ;; tab-width defaults to 2 globally
 tab-width 2
 ;; make indent commands use space only
 indent-tabs-mode nil
 comment-empty-lines t
 ;; Minimum width before truncate-line occurs
 truncate-partial-width-windows 40
 ;; Default line number width.
 ;; display-line-numbers-width 3

 ;; if set, when buffer size (not the number of lines!) exceeds 500;
 ;; then line number will not show in modeline; instead you get "??"
 ;;line-number-display-limit 500
 ;; hilight line only in current/selected window
 hl-line-sticky-flag nil
 require-final-newline t
 ;; case insensitive completion
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 ;; use short answer
 read-answer-short t
 ;; move cursor to top/bottom before signaling a scroll error
 scroll-error-top-bottom t
 ;; see its explanation in help doc
 epa-pinentry-mode 'loopback
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 ;; eldoc idle delay
 eldoc-idle-delay 1
 ispell-program-name "aspell"
 debug-on-error t
 ;; log errors and ingnore warnings
 ;; but not pop up the *Warnings* buffer
 warning-minimum-level :error
 native-comp-async-report-warnings-errors 'silent
 ;; >= emacs 28
 use-short-answers t)
 ;;inhibit-compacting-font-caches t

;; (fset 'yes-or-no-p 'y-or-n-p)
;; cutome funs to be hooked to various modes
(defun +add-margins-to-textmode ()
  "When in text-mode, add margins to both sides of the current buffer."
  (unless (memq major-mode '(mhtml-mode css-mode))
    (setq left-margin-width 4)
    (setq right-margin-width 4)))

;; Instead of enabling a minor mode globally
;; hook it to several major modes
(dolist (hook '(prog-mode-hook conf-space-mode-hook))
  (add-hook hook 'visual-line-mode)
  (add-hook hook 'hl-line-mode)
  ;; (add-hook hook 'display-line-numbers-mode)  ; line number on the left margin
  (add-hook hook 'column-number-mode)   ; for col numb on modeline -- Emacs 28
  (add-hook hook 'line-number-mode)
  (add-hook hook 'hs-minor-mode)
  (add-hook hook 'show-paren-mode))

(dolist (hook '(text-mode-hook))
  (add-hook hook 'visual-line-mode)
  (add-hook hook 'hl-line-mode)
  (add-hook hook 'column-number-mode)   ; for col numb on modeline -- Emacs 28
  (add-hook hook 'line-number-mode)
  (add-hook hook 'flyspell-mode)
  (add-hook hook #'+add-margins-to-textmode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'init-options)
;;; init-options ends here
