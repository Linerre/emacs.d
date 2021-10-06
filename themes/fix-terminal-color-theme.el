;;; --------------- lexical-binding: t -------------

;; Minor fixes for terminals (e.g. Alacritty) to display the correct colors
(deftheme fix-terminal-color
    "A minor fix to deal with alacritty's too yellow background issue.")

(custom-theme-set-faces
   'fix-terminal-color
   '(company-tooltip ((t (:background "#fbf1c7" :foreground "#3c383d"))))
   '(mode-line ((t (:background "#bdae93 " :foreground "#3c383d"))))
)

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'fix-terminal-color)
