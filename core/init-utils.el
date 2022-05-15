;; -*- lexical-binding: t -*-;
;; Commentary:
;; Interactive commands and con defined here for all modes
;; For fns and consts to use in other modules, refer to init-macros.el
;; Code:

;; straight-use-package is too much to type
;; (defalias 'emerge 'straight-use-package)
(defalias 'sup 'straight-use-package "Another alias for straight-use-package.")

;; which-key
(sup 'which-key)
(require 'which-key)
(which-key-mode)

;; vundo
(sup 'vundo)

;;  VARIABLES
(defconst *is-win* (string-equal system-type "windows-nt"))
(defconst *is-mac* (string-equal system-type "darwin"))
(defconst *is-linux* (string-equal system-type "gnu/linux"))

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

;; PROJECTS ROOT
(defconst projects-root "~/projects/")

;; This fn, when used in modeline, often throws an error:
;; Error during redisplay: (eval (+last-three-level-dir buffer-file-name)) signaled (wrong-type-argument stringp nil) [36 times]
;; This is because some buffers like *scratch*, *Message*,
;; *Help* are special; they point to no real file at all!
;; Consider making it interactive
(defun +trim-file-path (filepath)
  "Trim the absolute file path to the last three levels."
  (if (stringp filepath)
      (string-join
       (last (split-string  filepath "/") 3)
       "/")
    mode-line-buffer-identification))

(defun +project-indicator (fpath)
  "Enhance +trim-file-path and returns a shorter path in the format of `project-root':`current-buffer-name'. When it is not in ~/projects/, or in one of the special buffers, fall back to `mode-line-buffer-identification'."
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
             'face '(:foreground "#4C7A90")) ;azure4
           mode-line-buffer-identification)
        )
    mode-line-buffer-identification))

(provide 'init-utils)
;; init-utils.el ends here
