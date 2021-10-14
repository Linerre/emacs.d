;;; -*- lexical-binding: t -*-

;;; This small theme is meant to block all the headline colors in org mode
;;; plus a few other tweaks so that org mode looks far less colorful

(deftheme roam-research-default "A simple black-and-white-based theme that mimics roam research's default style")

(custom-theme-set-faces
 'roam-research-default
 ;; use nord's dark colors for now
 ;; TODO: bind colors to vars
 '(org-level-3 ((t (:foreground "black" :bold nil))))
 '(org-level-4 ((t (:foreground "black" :bold nil))))
 '(org-level-5 ((t (:foreground "#2e3440" :bold nil))))
 '(org-link ((t (:foreground "#1e90ff" :bold nil)))))


(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'roam-research-default)
