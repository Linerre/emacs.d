;;; corfu
(autoload #'corfu-mode "corfu" nil t)
(with-eval-after-load "corfu"
  (setq corfu-cycle t)                  ; Enable cycling
  (setq corfu-auto t)                   ; Enable auto completion
  (setq corfu-auto-delay 1)
  (setq corfu-preview-current nil) ; Disable current candidate preview
  (setq corfu-preselect 'prompt)   ; Preselect the prompt
  (setq corfu-on-exact-match 'insert) ; Configure handling of exact matches
  (setq corfu-scroll-margin 5)        ; Use scroll margin
  (setq corfu-max-width 60)           ; make width for popup
  (setq tab-always-indent 'complete)
  (setq corfu-quit-at-boundary 'separator)
  (setq-local completion-styles '(basic)))

;;; cape
(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-file)
(add-hook 'completion-at-point-functions #'cape-elisp-block)
(add-hook 'completion-at-point-functions #'cape-elisp-symbol)
