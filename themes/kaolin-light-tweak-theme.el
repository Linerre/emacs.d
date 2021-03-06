;;; -*- lexical-binding: t -*-

;;; This small theme provide the following tweaks:
;;; 1. Block all headline colors under level 2
;;; 2. Make org mode look more like roam research
;;; 3. Other minor tweaks suitable for all the modes in use

(deftheme kaolin-light-tweak "A simple black-and-white-based theme that mimics roam research's default style")

(custom-theme-set-faces
 'kaolin-light-tweak
 ;; use nord's dark colors for now
 ;; TODO: bind colors to vars
 ;; '(company-scrollbar-bg   ((t (:foreground "#DED4CD"))))
 ;; '(company-scrollbar-fg   ((t (:foreground "black"))))
 '(company-tooltip-scrollbar-track ((t (:background "#DED4CD"))))
 '(company-tooltip-scrollbar-thumb ((t (:background "black"))))
 '(meow-keypad-indicator  ((t (:bold t))))
 '(meow-insert-indicator  ((t (:bold t))))
 '(meow-normal-indicator  ((t (:bold t))))
 '(meow-motion-indicator  ((t (:bold t))))
 ;; '(mode-line ((t (:foreground "color-248"))))
 '(mode-line-inactive ((t (:foreground "color-249" ))))
 '(web-mode-html-attr-name-face ((t (:foreground "#845A84"))))
 '(web-mode-html-attr-equal-face ((t (:foreground "black"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "black"))))
 '(org-level-2 ((t (:bold t))))
 '(org-level-3 ((t (:foreground "black" :bold t))))
 '(org-level-4 ((t (:foreground "black" :bold t))))
 '(org-level-5 ((t (:foreground "black"))))
 ;; roam research backlink color: #106ba3 or #337ab7
 '(org-link ((t (:foreground "#337AB7"))))
 ;; foregroud UChicargo maron;
 '(org-code ((t (:foreground "#800000" :family "Monospace Serif" :bold nil))))
 ;; #A7B6C2 is too pale
 '(org-tag ((t (:foreground "#9BAFBF"))))
 '(org-special-keyword ((t (:foreground "#106BA3"))))
 ;; foreground roam bold;
 '(org-verbatim ((t (:foreground "#5C7080" :bold t :height 0.9)))))


(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'kaolin-light-tweak)
