;; Various emacs built-in options set for both GUI and TUI
;; startup config
(setq-default
 inhibit-startup-message t
 inhibit-startup-screen t
 inhibit-splash-screen t
 inhibit-x-resources t
 inhibit-startup-echo-area-message t
 initial-scratch-message ""
 ;; frame, window, buffer config
 ;; prefer horizental split
 split-height-threshold nil
 ;; split-width-threshold 100
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
 ;; create-lockfiles nil
 ;; mouse yank at point instead of click position.
 mouse-yank-at-point t
 ;; this fix the cursor movement lag
 auto-window-vscroll nil
 ;; The nano style for truncated long lines.
 auto-hscroll-mode 'current-line
 ;; scrolling
 scroll-conservatively 200 ; use thiner than default(6) window divider
 scroll-margin 1
 window-divider-default-right-width 1
 window-divider-default-bottom-width 1
 echo-keystrokes 0.01 ; no overline margin
 overline-margin 0
 tab-width 4
 indent-tabs-mode nil
 backward-delete-function nil ; DO NOT expand tabs when deleting
 comment-empty-lines t
 ;; Minimum width before truncate-line occurs
 truncate-partial-width-windows 40
 read-process-output-max (* 1024 1024)
 ;; hilight line only in current/selected window
 hl-line-sticky-flag nil
 require-final-newline t
 ;; case insensitive completion
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 ;; use short answer (>= emacs 28)
 read-answer-short t
 use-short-answers t
 ;; move cursor to top/bottom before signaling a scroll error
 scroll-error-top-bottom t
 epa-pinentry-mode 'loopback
 eldoc-idle-delay 1
 debug-on-error t
 ;; log errors and ingnore warnings
 ;; but not pop up the *Warnings* buffer
 warning-minimum-level :error
 native-comp-async-report-warnings-errors 'silent
 ;;inhibit-compacting-font-caches t
 ;; custom-file (expand-file-name "custom.el" user-emacs-directory)
  ;; show match number in minibuffer
 isearch-lazy-count t
 list-matching-lines-default-context-lines 1
 ;; font-lock level
 font-lock-maximum-decoration 2
 treesit-font-lock-level 2
 bidi-display-reordering nil
 enable-local-variables :safe
 cursor-in-non-selected-windows nil
 ;; in dired buffers: `C-s' == `M-s f C-s'
 dired-isearch-filenames t
 ;; disable emacs input method and use the system one
 x-input-method-use-protocol nil
 default-input-method nil
 gtk-use-im-context nil
 icon-preference '(text symbol)
 vc-handled-backends '(Git SVN)
 dired-listing-switches "-alh --group-directories-first"
 default-directory "~/projects")

;; Save buffers on gain/loss of focus
(defun +real-auto-save ()
  (interactive)
  (save-some-buffers t))

(setq after-focus-change-function '+real-auto-save)


;; Instead of enabling a minor mode globally
;; hook it to several major modes
(dolist (hook '(conf-space-mode-hook prog-mode-hook))
  (add-hook hook #'visual-line-mode)
  (add-hook hook #'column-number-mode)   ; for col numb on modeline -- Emacs 28
  (add-hook hook #'line-number-mode)
  (add-hook hook #'electric-pair-local-mode)
  (add-hook hook #'show-paren-mode))

;; In prog-mode or any its deried modes, make isearch case sensitive
(add-hook 'prog-mode-hook (lambda ()
                            (setq case-fold-search nil)))

(add-hook 'conf-mode-hook #'electric-pair-local-mode)
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'column-number-mode)
(add-hook 'text-mode-hook #'line-number-mode)

(dolist (hook '(org-mode-hook markdown-mode-hook LaTeX-mode-hook))
  (add-hook hook #'flyspell-mode)
  (add-hook hook #'yas-minor-mode))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defvar parameters                      ; cause side buffers to not be closed
  '(window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t))))

(setq switch-to-buffer-in-dedicated-window 'pop
      ;; window-resize-pixelwise t
      fit-window-to-buffer-horizontally t)

;; (with-eval-after-load 'window
;;   (setq
;;    display-buffer-alist
;;    '(
    ;; ("\\*Buffer List\\*" display-buffer-in-side-window
    ;;  (side . top) (slot . 0) (window-height . 0.25)
    ;;  (preserve-size . (nil . t))
    ;;  ;; ,parameters
    ;;  )
    ;; ("\\*Tags List\\*" display-buffer-in-side-window
    ;;  (side . right) (slot . 0) (window-width . fit-window-to-buffer)
    ;;  (preserve-size . (t . nil))
    ;;  ;; ,parameters
    ;;  )
    ;; ("\\*\\(?:help\\|grep\\|Completions\\)\\*"
    ;;  display-buffer-in-side-window
    ;;  (side . bottom) (slot . -1) (preserve-size . (nil . t))
    ;;  ;; ,parameters
    ;;  )
    ;; ("\\*\\(e?shell\\|compilation\\)\\*" display-buffer-in-side-window
    ;;  (side . bottom) (slot . 1) (preserve-size . (nil . t))
    ;;  ;; ,parameters
    ;;  ))))

;; Rebind a few keys
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(define-key global-map (kbd "C-x C-b") #'ibuffer)
(add-hook 'ibuffer-mode-hook #'hl-line-mode)
;; map C-[ to escape key event
(when (display-graphic-p)
  (define-key input-decode-map [?\C-\[] [escape]))

;; auto pair
(autoload #'paredit-mode "paredit" nil t)

(dolist (h '(c-mode-hook
             c++-mode-hook
             css-mode-hook
             eat-mode-hook
             go-mode-hook
             jinja2-mode-hook
             js-mode-hook
             lua-mode-hook
             nix-mode-hook
             org-mode-hook
             python-mode-hook
             sql-mode-hook
             cider-repl-mode-hook))
  (add-hook h #'electric-pair-local-mode))

(dolist (h '(clojure-mode-hook
             clojurescript-mode-hook
             emacs-lisp-mode-hook
             lisp-mode-hook
             lisp-interaction-mode-hook))
  (add-hook h #'paredit-mode))

;; I don't use these keybindings in paredit but I need them globally
(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "M-<up>") nil)
  (define-key paredit-mode-map (kbd "M-<down>") nil)
  (define-key paredit-mode-map (kbd "C-M-d") nil)
  (define-key paredit-mode-map (kbd "C-M-u") nil))

(provide 'init-options)
;;; init-options.el ends here
