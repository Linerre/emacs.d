;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code
(straight-use-package
 '(meow :type git :host github :repo "DogLooksGood/meow"))
(straight-use-package
 '(ford-this.el :type git :host github :repo "magnars/fold-this.el"))

(autoload #'fold-this "fold-this" nil t)
;; meow
(setq meow-expand-hint-remove-delay 1.5
      ;;meow-use-cursor-position-hack t
      meow-replace-state-name-list
      '((normal . "NORMAL")
        (insert . "INSERT")
        (keypad . "KEYPAD")
        (motion . "MOTION")
        (beacon . "BEACON")))

(require 'meow)

(meow-global-mode 1)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  ;; use C-n/p to move up/down in Motion mode instead
  ;; Otherwise j/k will not be recognized as characters in gdb mode
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   ;;'("j" . meow-motion-origin-command)
   ;;'("k" . meow-motion-origin-command)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   ;; cheatsheet
   '("?" . meow-cheatsheet)
   ;; high frequency keybindings
   ;'("F" . "C-h")
   ;'(")" . "C-)")
   ;'("}" . "C-}")
   '("." . "M-.")
   '("," . "M-,")
   '("<escape>" . "C-g")
   ;; window management
   '("w" . windmove-up)
   '("a" . windmove-left)
   '("s" . windmove-down)
   '("d" . windmove-right)
   '("r w" . windmove-delete-up)
   '("r a" . windmove-delete-left)
   '("r s" . windmove-delete-down)
   '("r d" . windmove-delete-right)
   ;'("W" . window-swap-states)
   '("o" . delete-other-windows)
   '("=" . split-window-right)
   '("-" . split-window-below)
   ;; high frequency commands
   '("e" . "C-x C-e")
   '("<SPC>" . "C-x C-s")
   ;; '("$" . +change-theme)
   '(";" . comment-dwim)
   '("k" . kill-this-buffer)
   '("p" . project-find-file)
   '("j" . project-switch-to-buffer)
   ;'("d" . dired)
   '("b" . switch-to-buffer)
   ;; '("r" . rg-project)
   '("f" . find-file)
   '("i" . imenu)
   '("n" . "M-x")
   '("g" . "C-x g")
   '("z" . fold-this)
   '("Z" . fold-this)
   ;; toggles
   '("L" . display-line-numbers-mode)
   '("S" . smartparens-strict-mode)
   '("t" . telega)
   '("P" . pass)
   '("A" . org-agenda)
   ;; org
   '("U" . "C-u C-u")
   '("C" . "C-c C-c")
   '("X" . "C-c C-x"))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-C-d)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("g" . meow-cancel)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("N" . meow-pop-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("V" . meow-kmacro-matches)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-kmacro-lines)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("'" . repeat)
   '("\\" . quoted-insert)))
   ;'("<escape>" . meow-last-buffer)))

(setq
 meow-visit-sanitize-completion nil
 meow-esc-delay 0.001
 meow-keypad-describe-delay 0.5
 meow-select-on-change t
 meow-cursor-type-normal 'box
 meow-cursor-type-insert '(bar . 4)
 meow-selection-command-fallback
 '((meow-replace . meow-page-up)
   (meow-change . meow-change-char)
   (meow-save . meow-save-empty)
   (meow-kill . meow-C-k)
   (meow-cancel . keyboard-quit)
   (meow-pop . meow-pop-grab)
   (meow-delete . meow-C-d)
   (meow-beacon-change . meow-beacon-change-char))
 )

(require 'meow)

(meow-global-mode 1)

(with-eval-after-load "meow"
  ;; make Meow usable in TUI Emacs
  (meow-esc-mode 1)
  ;; (add-to-list 'meow-mode-state-list '(inf-iex-mode . normal))
  (add-to-list 'meow-mode-state-list '(authinfo-mode . normal))
  (add-to-list 'meow-mode-state-list '(bibtex-mode . normal))
  (add-to-list 'meow-mode-state-list '(gud-mode . normal))
  (add-to-list 'meow-mode-state-list '(shell-mode . normal))
  ;; (add-to-list 'meow-mode-state-list '(Custom-mode . normal))
  ;; (add-to-list 'meow-mode-state-list '(cider-test-report-mode . normal))
  (add-to-list 'meow-grab-fill-commands 'eval-expression)
  (setq meow-cursor-type-keypad 'box)
  (setq meow-cursor-type-insert '(bar . 2))
  ;; use << and >> to select to bol/eol
  (add-to-list 'meow-char-thing-table '(?> . line))
  (add-to-list 'meow-char-thing-table '(?< . line))
  ;; define our command layout
  (meow-setup)
  ;; add indicator to modeline
  (meow-setup-indicator))

(provide 'init-meow)
