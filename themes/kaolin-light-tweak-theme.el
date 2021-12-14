;;; -*- lexical-binding: t -*-

;;; This small theme provide the following tweaks:
;;; 1. Block all headline colors under org level 2
;;; 2. Make org mode look more like roam research
;;; 3. Other minor tweaks suitable for all the modes in use

(deftheme kaolin-light-tweak
  "A after-hook theme that makes kaolin-light less colorful.")

(custom-theme-set-faces
 'kaolin-light-tweak
 ;; use nord's dark colors for now
 ;; TODO: bind colors to vars
 '(meow-keypad-indicator  ((t (:bold t))))
 '(meow-insert-indicator  ((t (:bold t))))
 '(meow-normal-indicator  ((t (:bold t))))
 '(meow-motion-indicator  ((t (:bold t))))
 '(meow-bmacro-indicator  ((t (:bold t))))
 '(mode-line              ((((tty)) :background "green" :foreground "black")
                           (((graphic)) :background "grey70")))
 '(mode-line-inactive     ((((tty)) :background "yellow" :foreground "black")
                           (((graphic)) :background "grey80")))
 '(org-level-2 ((t (:bold t))))
 '(org-level-3 ((t (:foreground "black"))))
 '(org-level-4 ((t (:foreground "black"))))
 '(org-level-5 ((t (:foreground "black"))))
 ;; roam research backlink color
 '(org-link ((t (:foreground "#106ba3"))))
 ;; foregroud UChicargo maron;
 '(org-code ((t (:foreground "#800000" :family "Monospace Serif" :bold nil))))
 ;; foreground roam bold;
 '(org-verbatim ((t (:foreground "#5c7080" :bold t :height 0.9)))))


(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'kaolin-light-tweak)
