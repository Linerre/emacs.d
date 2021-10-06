;;; --------------- lexical-binding: t -------------

;; Minor fixes for terminals (e.g. Alacritty) to display the correct colors. Clean up later
(deftheme fix-terminal-color
    "A minor fix to deal with alacritty's too yellow background issue.")

(custom-theme-set-faces
   'fix-terminal-color
   '(company-tooltip ((t (:background "#fbf1c7" :foreground "#3c383d"))))
   '(mode-line ((t (:background "#bdae93 " :foreground "#3c383d"))))
   '(font-lock-comment-face ((t (:foreground "#a89984"))))
   '(font-lock-function-name-face ((t (:foreground "#458588"))))
   '(font-lock-keyword-face ((t (:foreground "#cc241d"))))
   '(font-lock-constant-face ((t (:foreground "#381a8a"))))
   '(font-lock-string-face ((t (:foreground "#98971a"))))
   '(font-lock-builtin-face ((t (:foreground "#b16286"))))
)

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'fix-terminal-color)
