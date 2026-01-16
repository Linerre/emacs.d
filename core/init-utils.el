;;; -*- lexical-binding: t -*-;
;;; Commentary:
;;; Interactive commands defined here for all modes, plus tools such as eat, gptel, rg, etc.
;;; Code:

;; TIME FUNCS
(defun +insert-timestamp ()
  "Insert timestamp of the current point in time."
  (interactive)
  (insert (current-time-string)))

(defun +insert-today-date-string ()
  "Insert today's date in the format of YYYY-MM-DD WEK."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a" (current-time))))

(defun +insert-tomorrow-date-string ()
  "Insert tomorrow's date in the format of YYYY-MM-DD WEK."
  (interactive)
  (setq tmw (+ (* 24 60 60) (time-convert nil 'integer)))
  (insert (format-time-string "%Y-%m-%d %a" tmw)))

(global-set-key (kbd "C-c t s") #'+insert-timestamp)
(global-set-key (kbd "C-c t d") #'+insert-today-date-string)
(global-set-key (kbd "C-c t m") #'+insert-tomorrow-date-string)

;; This fn, when used in modeline, often throws an error:
;; Error during redisplay: (eval (+last-three-level-dir buffer-file-name)) signaled (wrong-type-argument stringp nil) [36 times]
;; This is because some buffers like *scratch*, *Message*,
;; *Help* are special; they point to no real file at all!
;; Consider making it interactive
(defun +trim-file-path (filepath)
  "Trim the absolute FILEPATH to the last three levels."
  (if (stringp filepath)
      (string-join
       (last (split-string  filepath "/") 3)
       "/")
    mode-line-buffer-identification))

(defun +project-indicator (fpath)
  "Enhance +trim-file-path and return a shorter FPATH in the format of `project-root':`current-buffer-name'.
When it is not in ~/projects/, or in one of the special buffers, fall back to `mode-line-buffer-identification'."
  (if (stringp fpath)
      ;; For normal user, /home/username/projects
      ;; For root user, /root/projects
      (let* ((user-proj (nth 3 (split-string fpath "/")))
             (root-proj (nth 2 (split-string fpath "/")))
             (proj-dir (cond
                       ((string= "projects" user-proj)
                        (nth 4 (split-string fpath "/"))
                        )
                       ((string= "projects" root-proj)
                        (nth 3 (split-string fpath "/"))
                        )
                       (t mode-line-buffer-identification))))
        (if (stringp proj-dir)
            (propertize
             (string-join
              (list
               proj-dir
               (buffer-name))
              ":")
             'face '(:foreground "#5CC7D1"))
           mode-line-buffer-identification)
        )
    mode-line-buffer-identification))

;; Highlight TODO/FIMEXE in comments
(defun +hl-cmt-kws ()
  "Hilight keywords such as TODO and FIXME in comments."
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|TODO\\):" 1 font-lock-warning-face prepend))))

(defun +move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun +move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun +move-region-up ()
  "Move the selected region up by one line, swapping the top line with the line above."
  (interactive)
  (when (use-region-p)
    (let ((start (region-beginning))
          (end (region-end))
          (column (- (point) (line-beginning-position))))
      (save-excursion
        (goto-char start)
        (beginning-of-line)
        (setq start (point))
        (goto-char end)
        (end-of-line)
        (setq end (point))
        ;; Swap the top line of the region with the line above
        (transpose-lines 1)
        ;; Adjust the region boundaries
        (forward-line -1)
        (setq start (point))
        (forward-line (count-lines start end))
        (setq end (point)))
      ;; Restore the active selection
      (set-mark start)
      (goto-char end)
      (forward-char column))))

;; misc
;; rexim/dotfiles/blob/3011cc1769e769ce4c65d22a46f66ac3e8fc81e1/.emacs.rc/misc-rc.el#L120
(defun rc/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-,") #'rc/duplicate-line)
(global-set-key (kbd "M-<up>") #'+move-line-up)
(global-set-key (kbd "M-<down>") #'+move-line-down)
(global-set-key (kbd "C-c C-d") #'duplicate-line)

;;; embark
(global-set-key (kbd "C-'") #'embark-act)
(global-set-key (kbd "C-;") #'embark-dwim)

;;; eat
(add-hook 'eshell-load-hook #'eat-eshell-mode)
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
(with-eval-after-load 'eat-eshell-mode
  (meow-mode -1))

;;; rg
(autoload #'rg "rg" nil t)

;;; citre
(autoload #'citre-mode "citre" nil t)

(with-eval-after-load "citre"
  (require 'citre-config)
  (setq citre-default-create-tags-file-location 'global-cache
        citre-readtags-program "/usr/bin/readtags"
        citre-ctags-program "/usr/bin/ctags"
        citre-tags-file-names '("tags")
        citre-enable-backends '(ctags global eglot)
        citre-peek-backends '(tags)
        citre-auto-enable-citre-mode-modes '(c-ts-mode
                                             jtsx-tsx-mode
                                             jtsx-jsx-mode
                                             move-mode
                                             typescript-ts-mode
                                             rust-mode
                                             rust-ts-mode))
  (define-key citre-mode-map (kbd "C-x c j") #'citre-jump)
  (define-key citre-mode-map (kbd "C-x c J") #'citre-jump-back)
  (define-key citre-mode-map (kbd "C-x c p") #'citre-peek)
  (define-key citre-mode-map (kbd "C-x c a") #'citre-ace-peek)
  (define-key citre-mode-map (kbd "C-x c u") #'citre-update-this-tag-file))

(gptel-make-deepseek "DeepSeek"
  :stream t
  :key (lambda () (password-store-get "Dev/deepseek"))
  :models '(deepseek-chat deepseek-reasoner))

(gptel-make-openai "Moonshot"
  :host "api.moonshot.cn"
  :key (lambda () (password-store-get "Dev/kimik2"))
  :stream t
  :models '(kimi-k2-0905-preview kimi-latest kimi-k2-0711-preview))

(gptel-make-anthropic "Claude"
  :stream t
  :models '(claude-sonnet-4-5-20250929
            claude-sonnet-4-20250514)
  :key (lambda () (password-store-get "Dev/claude-key1")))

(setq gptel-default-mode 'markdown-mode
      gptel-prompt-prefix-alist
      '((markdown-mode . "## ") (org-mode . "** ") (text-mode . ">> "))
      gptel-model 'kimi-latest
      gptel-backend (gptel-make-openai "Moonshot"
                      :host "api.moonshot.cn"
                      :key (lambda () (password-store-get "Dev/kimik2"))
                      :stream t
                      :models '(kimi-latest kimi-k2-0905-preview kimi-k2-0711-preview)))

;;; Email
;; SMTP configuration
(setq auth-sources '("~/.authinfo.gpg"))
(setq user-full-name "Errenil Noel")
(setq user-mail-address "errelin.aaron@gmail.com")
(setq gnus-select-method '(nntp "news.gmane.io"))
(setq send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls)

;;; Need Emacs >= 30
(setq which-key-idle-delay 2)
(which-key-mode 1)

;;; ansi color
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;;; envrc
(require 'envrc)
(setq envrc-show-summary-in-minibuffer nil)
(add-hook 'after-init-hook 'envrc-global-mode)

;;; outline-minor-mode
(defun +set-outline-regexp ()
  (declare (native-compile t))
  (cond
   ((eq major-mode 'rust-mode)
    (setq-local outline-regexp "#\\[\\|//!?"))

   ((or (eq major-mode 'clojure-mode)
        (eq major-mode 'clojure-ts-mode)
        (eq major-mode 'emacs-lisp-mode))
    (setq-local outline-regexp (concat  outline-regexp  "\\|;;--")))))

(add-hook 'outline-minor-mode-hook #'+set-outline-regexp)

;;; jinx and enchant to add a new word to personal dict
(defun add-word-to-personal-dictionary (&optional word)
  "Add a word to the personal enchant dictionary.
If WORD is not provided, prompt the user for input.
The word at point is used as the default if available."
  (interactive)
  (let* ((word-at-point (thing-at-point 'word t))
         (default-word (when word-at-point (downcase (string-trim word-at-point))))
         (prompt (if default-word
                     (format "Add word to dictionary (default: %s): " default-word)
                   "Add word to dictionary: "))
         (input-word (or word
                        (read-string prompt nil nil default-word)))
         (lang "en_US")
         (dict-file (expand-file-name (format "~/.config/enchant/%s.dic" lang))))

    ;; Validate input
    (when (or (null input-word) (string-empty-p (string-trim input-word)))
      (user-error "No word provided"))

    (let ((clean-word (downcase (string-trim input-word)))
          (word-exists-p nil))

      ;; Create directory if it doesn't exist
      (unless (file-directory-p (file-name-directory dict-file))
        (make-directory (file-name-directory dict-file) t))

      ;; Check if word already exists in dictionary (case-insensitive)
      (when (file-exists-p dict-file)
        (with-temp-buffer
          (insert-file-contents dict-file)
          (goto-char (point-min))
          (when (re-search-forward (format "^%s$" (regexp-quote clean-word)) nil t)
            (setq word-exists-p t))))

      ;; Either add the word or show it already exists
      (if word-exists-p
          (message "Word '%s' already exists in personal dictionary" clean-word)
        ;; Append word to dictionary file
        (with-temp-buffer
          (when (file-exists-p dict-file)
            (insert-file-contents dict-file))
          (goto-char (point-max))
          ;; Ensure we're on a new line
          (unless (or (bobp) (bolp))
            (insert "\n"))
          (insert clean-word "\n")
          (write-region (point-min) (point-max) dict-file nil 'silent))

        (message "Added '%s' to personal dictionary: %s" clean-word dict-file)))))

;; Optional: Add a keybinding for convenience
;; (global-set-key (kbd "C-c d a") #'add-word-to-personal-dictionary)


(provide 'init-utils)
;;; init-utils.el ends here
