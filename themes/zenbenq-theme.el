;;; zenburn-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2020 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/bbatsov/zenburn-emacs
;; Version: 2.8.0-snapshot

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A port of the popular Vim theme Zenburn for Emacs 24+, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Jani Nurminen created the original theme for vim on which this port
;; is based.

;;; Code:

(deftheme zenbenq "The Zenburn color theme")

(defgroup zenbenq-theme nil
  "Zenburn theme."
  :group 'faces
  :prefix "zenburn-"
  :link '(url-link :tag "GitHub" "http://github.com/bbatsov/zenburn-emacs")
  :tag "Zenburn theme")

;;;###autoload
(defcustom zenbenq-override-colors-alist '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'zenbenq-theme
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(defvar zenbenq-use-variable-pitch nil
  "When non-nil, use variable pitch face for some headings and titles.")

(defvar zenbenq-scale-org-headlines nil
  "Whether `org-mode' headlines should be scaled.")

(defvar zenbenq-scale-outline-headlines nil
  "Whether `outline-mode' headlines should be scaled.")

(defcustom zenbenq-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'zenbenq-theme
  :package-version '(zenbenq . "2.6"))

(defcustom zenbenq-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'zenbenq-theme
  :package-version '(zenbenq . "2.6"))

(defcustom zenbenq-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'zenbenq-theme
  :package-version '(zenbenq . "2.6"))

(defcustom zenbenq-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'zenbenq-theme
  :package-version '(zenbenq . "2.6"))

(defcustom zenbenq-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'zenbenq-theme
  :package-version '(zenbenq . "2.6"))

;;; Color Palette

(defvar zenbenq-default-colors-alist
  '(("zenbenq-fg-1"     . "#656555")
    ("zenbenq-fg-05"    . "#989890")
    ("zenbenq-fg"       . "#DCDCCC")
    ("zenbenq-fg+1"     . "#FFFFEF")
    ("zenbenq-fg+2"     . "#FFFFFD")
    ("zenbenq-bg-2"     . "#000000")
    ("zenbenq-bg-1"     . "#2B2B2B")
    ("zenbenq-bg-08"    . "#303030")
    ("zenbenq-bg-05"    . "#383838")
    ("zenbenq-bg"       . "#3F3F3F")
    ("zenbenq-bg+05"    . "#494949")
    ("zenbenq-bg+1"     . "#4F4F4F")
    ("zenbenq-bg+2"     . "#5F5F5F")
    ("zenbenq-bg+3"     . "#6F6F6F")
    ("zenbenq-red-6"    . "#6C3333")
    ("zenbenq-red-5"    . "#7C4343")
    ("zenbenq-red-4"    . "#8C5353")
    ("zenbenq-red-3"    . "#9C6363")
    ("zenbenq-red-2"    . "#AC7373")
    ("zenbenq-red-1"    . "#BC8383")
    ("zenbenq-red"      . "#CC9393")
    ("zenbenq-red+1"    . "#DCA3A3")
    ("zenbenq-red+2"    . "#ECB3B3")
    ("zenbenq-orange"   . "#DFAF8F")
    ("zenbenq-yellow-2" . "#D0BF8F")
    ("zenbenq-yellow-1" . "#E0CF9F")
    ("zenbenq-yellow"   . "#F0DFAF")
    ("zenbenq-green-5"  . "#2F4F2F")
    ("zenbenq-green-4"  . "#3F5F3F")
    ("zenbenq-green-3"  . "#4F6F4F")
    ("zenbenq-green-2"  . "#5F7F5F")
    ("zenbenq-green-1"  . "#6F8F6F")
    ("zenbenq-green"    . "#7F9F7F")
    ("zenbenq-green+1"  . "#8FB28F")
    ("zenbenq-green+2"  . "#9FC59F")
    ("zenbenq-green+3"  . "#AFD8AF")
    ("zenbenq-green+4"  . "#BFEBBF")
    ("zenbenq-cyan"     . "#93E0E3")
    ("zenbenq-blue+3"   . "#BDE0F3")
    ("zenbenq-blue+2"   . "#ACE0E3")
    ("zenbenq-blue+1"   . "#94BFF3")
    ("zenbenq-blue"     . "#8CD0D3")
    ("zenbenq-blue-1"   . "#7CB8BB")
    ("zenbenq-blue-2"   . "#6CA0A3")
    ("zenbenq-blue-3"   . "#5C888B")
    ("zenbenq-blue-4"   . "#4C7073")
    ("zenbenq-blue-5"   . "#366060")
    ("zenbenq-magenta"  . "#DC8CC3"))
  "List of Zenbenq colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro zenbenq-with-color-variables (&rest body)
  "`let' bind all colors defined in `zenbenq-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append zenbenq-default-colors-alist
                           zenbenq-override-colors-alist))
         (z-variable-pitch (if zenbenq-use-variable-pitch
                               'variable-pitch 'default)))
     ,@body))

;;; Theme Faces
(zenbenq-with-color-variables
  (custom-theme-set-faces
   'zenbenq
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,zenbenq-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,zenbenq-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,zenbenq-fg :background ,zenbenq-bg))))
   `(cursor ((t (:background ,zenbenq-fg+1))))
   `(widget-field ((t (:foreground ,zenbenq-fg :background ,zenbenq-bg+3))))
   `(escape-glyph ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(fringe ((t (:foreground nil :background nil))))
   `(header-line ((t (:foreground ,zenbenq-yellow
                                  :background ,zenbenq-bg-1
                                  :box (:line-width -1 :style released-button)
                                  :extend t))))
   `(highlight ((t (:background ,zenbenq-bg-05))))
   `(hl ((t (:backgorund ,zenbenq-bg-05))))
   `(success ((t (:foreground ,zenbenq-green :weight bold))))
   `(warning ((t (:foreground ,zenbenq-orange :weight bold))))
   `(tooltip ((t (:foreground ,zenbenq-fg :background ,zenbenq-bg+1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,zenbenq-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,zenbenq-green))))
   `(compilation-error-face ((t (:foreground ,zenbenq-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,zenbenq-fg))))
   `(compilation-info-face ((t (:foreground ,zenbenq-blue))))
   `(compilation-info ((t (:foreground ,zenbenq-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,zenbenq-green))))
   `(compilation-line-face ((t (:foreground ,zenbenq-yellow))))
   `(compilation-line-number ((t (:foreground ,zenbenq-yellow))))
   `(compilation-message-face ((t (:foreground ,zenbenq-blue))))
   `(compilation-warning-face ((t (:foreground ,zenbenq-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,zenbenq-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,zenbenq-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,zenbenq-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,zenbenq-fg-1))))
   `(completions-common-part ((t (:foreground ,zenbenq-blue))))
   `(completions-first-difference ((t (:foreground ,zenbenq-fg+1))))
;;;;; customize
   `(custom-variable-tag ((t (:foreground ,zenbenq-blue :weight bold))))
   `(custom-group-tag ((t (:foreground ,zenbenq-blue :weight bold :height 1.2))))
   `(custom-state ((t (:foreground ,zenbenq-green+4))))
;;;;; display-fill-column-indicator
   `(fill-column-indicator ((,class :foreground ,zenbenq-bg-05 :weight semilight)))
;;;;; eww
   '(eww-invalid-certificate ((t (:inherit error))))
   '(eww-valid-certificate   ((t (:inherit success))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,zenbenq-fg))))
   `(grep-error-face ((t (:foreground ,zenbenq-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,zenbenq-blue))))
   `(grep-match-face ((t (:foreground ,zenbenq-orange :weight bold))))
   `(match ((t (:background ,zenbenq-bg-1 :foreground ,zenbenq-orange :weight bold))))
;;;;; hi-lock
   `(hi-blue    ((t (:background ,zenbenq-cyan    :foreground ,zenbenq-bg-1))))
   `(hi-green   ((t (:background ,zenbenq-green+4 :foreground ,zenbenq-bg-1))))
   `(hi-pink    ((t (:background ,zenbenq-magenta :foreground ,zenbenq-bg-1))))
   `(hi-yellow  ((t (:background ,zenbenq-yellow  :foreground ,zenbenq-bg-1))))
   `(hi-blue-b  ((t (:foreground ,zenbenq-blue    :weight     bold))))
   `(hi-green-b ((t (:foreground ,zenbenq-green+2 :weight     bold))))
   `(hi-red-b   ((t (:foreground ,zenbenq-red     :weight     bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,zenbenq-yellow-2 :weight bold :background ,zenbenq-bg+2))))
   `(isearch-fail ((t (:foreground ,zenbenq-fg :background ,zenbenq-red-4))))
   `(lazy-highlight ((t (:foreground ,zenbenq-yellow-2 :weight bold :background ,zenbenq-bg-05))))

   `(menu ((t (:foreground ,zenbenq-fg :background ,zenbenq-bg))))
   `(minibuffer-prompt ((t (:foreground ,zenbenq-yellow))))
   `(mode-line
     ((,class (:foreground ,zenbenq-green+1
                           :background ,zenbenq-bg-1))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,zenbenq-green-2
                      :background ,zenbenq-bg-05))))
   `(region ((,class (:background ,zenbenq-bg-1 :extend t))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,zenbenq-bg+2))))
   `(trailing-whitespace ((t (:background ,zenbenq-red))))
   `(vertical-border ((t (:foreground ,zenbenq-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,zenbenq-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,zenbenq-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,zenbenq-green-2))))
   `(font-lock-constant-face ((t (:foreground ,zenbenq-green+4))))
   `(font-lock-doc-face ((t (:foreground ,zenbenq-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,zenbenq-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,zenbenq-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,zenbenq-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,zenbenq-red))))
   `(font-lock-type-face ((t (:foreground ,zenbenq-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,zenbenq-orange))))
   `(font-lock-warning-face ((t (:foreground ,zenbenq-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; line numbers (Emacs 26.1 and above)
   `(line-number ((t (:foreground ,zenbenq-bg+3 :background ,zenbenq-bg-05))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,zenbenq-yellow-2))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,zenbenq-fg))))
   `(newsticker-default-face ((t (:foreground ,zenbenq-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,zenbenq-green+3))))
   `(newsticker-extra-face ((t (:foreground ,zenbenq-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,zenbenq-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,zenbenq-green))))
   `(newsticker-new-item-face ((t (:foreground ,zenbenq-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,zenbenq-red))))
   `(newsticker-old-item-face ((t (:foreground ,zenbenq-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,zenbenq-fg))))
   `(newsticker-treeview-face ((t (:foreground ,zenbenq-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,zenbenq-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,zenbenq-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,zenbenq-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,zenbenq-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,zenbenq-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,zenbenq-bg-1 :foreground ,zenbenq-yellow))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))
;;;; Third-party
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,zenbenq-green+1))))
   `(android-mode-error-face ((t (:foreground ,zenbenq-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,zenbenq-fg))))
   `(android-mode-verbose-face ((t (:foreground ,zenbenq-green))))
   `(android-mode-warning-face ((t (:foreground ,zenbenq-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,zenbenq-cyan :weight bold))))
   `(anzu-mode-line-no-match ((t (:foreground ,zenbenq-red :weight bold))))
   `(anzu-match-1 ((t (:foreground ,zenbenq-bg :background ,zenbenq-green))))
   `(anzu-match-2 ((t (:foreground ,zenbenq-bg :background ,zenbenq-orange))))
   `(anzu-match-3 ((t (:foreground ,zenbenq-bg :background ,zenbenq-blue))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,zenbenq-yellow))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,zenbenq-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,zenbenq-yellow))))
   `(font-latex-italic-face ((t (:foreground ,zenbenq-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,zenbenq-orange))))
   `(font-latex-script-char-face ((t (:foreground ,zenbenq-orange))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,zenbenq-red))))
   `(agda2-highlight-symbol-face ((t (:foreground ,zenbenq-orange))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,zenbenq-blue-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,zenbenq-fg))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,zenbenq-fg))))
   `(agda2-highlight-datatype-face ((t (:foreground ,zenbenq-blue))))
   `(agda2-highlight-function-face ((t (:foreground ,zenbenq-blue))))
   `(agda2-highlight-module-face ((t (:foreground ,zenbenq-blue-1))))
   `(agda2-highlight-error-face ((t (:foreground ,zenbenq-bg :background ,zenbenq-magenta))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,zenbenq-bg :background ,zenbenq-magenta))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,zenbenq-bg :background ,zenbenq-magenta))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,zenbenq-bg :background ,zenbenq-magenta))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,zenbenq-bg :background ,zenbenq-magenta))))
   `(agda2-highlight-typechecks-face ((t (:background ,zenbenq-red-4))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,zenbenq-fg-1 :background ,zenbenq-bg :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,zenbenq-green+3 :background ,zenbenq-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,zenbenq-yellow :background ,zenbenq-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,zenbenq-red+1 :background ,zenbenq-bg :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,zenbenq-cyan :background ,zenbenq-bg :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,zenbenq-fg :background ,zenbenq-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,zenbenq-orange :background ,zenbenq-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,zenbenq-orange :background ,zenbenq-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,zenbenq-fg :background ,zenbenq-bg-1))))
   `(company-tooltip-mouse ((t (:background ,zenbenq-bg-1))))
   `(company-tooltip-common ((t (:foreground ,zenbenq-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,zenbenq-green+2))))
   `(company-scrollbar-fg ((t (:background ,zenbenq-bg-1))))
   `(company-scrollbar-bg ((t (:background ,zenbenq-bg+2))))
   `(company-preview ((t (:background ,zenbenq-green+2))))
   `(company-preview-common ((t (:foreground ,zenbenq-green+2 :background ,zenbenq-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,zenbenq-yellow-1 :foreground ,zenbenq-bg))))
   `(bm-fringe-face ((t (:background ,zenbenq-yellow-1 :foreground ,zenbenq-bg))))
   `(bm-fringe-persistent-face ((t (:background ,zenbenq-green-2 :foreground ,zenbenq-bg))))
   `(bm-persistent-face ((t (:background ,zenbenq-green-2 :foreground ,zenbenq-bg))))
;;;;; calfw
   `(cfw:face-annotation ((t (:foreground ,zenbenq-red :inherit cfw:face-day-title))))
   `(cfw:face-day-title ((t nil)))
   `(cfw:face-default-content ((t (:foreground ,zenbenq-green))))
   `(cfw:face-default-day ((t (:weight bold))))
   `(cfw:face-disable ((t (:foreground ,zenbenq-fg-1))))
   `(cfw:face-grid ((t (:inherit shadow))))
   `(cfw:face-header ((t (:inherit font-lock-keyword-face))))
   `(cfw:face-holiday ((t (:inherit cfw:face-sunday))))
   `(cfw:face-periods ((t (:foreground ,zenbenq-cyan))))
   `(cfw:face-saturday ((t (:foreground ,zenbenq-blue :weight bold))))
   `(cfw:face-select ((t (:background ,zenbenq-blue-5))))
   `(cfw:face-sunday ((t (:foreground ,zenbenq-red :weight bold))))
   `(cfw:face-title ((t (:height 2.0 :inherit (variable-pitch font-lock-keyword-face)))))
   `(cfw:face-today ((t (:foreground ,zenbenq-cyan :weight bold))))
   `(cfw:face-today-title ((t (:inherit highlight bold))))
   `(cfw:face-toolbar ((t (:background ,zenbenq-blue-5))))
   `(cfw:face-toolbar-button-off ((t (:underline nil :inherit link))))
   `(cfw:face-toolbar-button-on ((t (:underline nil :inherit link-visited))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,zenbenq-orange :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,zenbenq-green+1))))
   `(cider-deprecated-face ((t (:background ,zenbenq-yellow-2))))
   `(cider-instrumented-face ((t (:box (:color ,zenbenq-red :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,zenbenq-cyan :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,zenbenq-red-4))))
   `(cider-test-error-face ((t (:background ,zenbenq-magenta))))
   `(cider-test-success-face ((t (:background ,zenbenq-green-2))))
   `(cider-fringe-good-face ((t (:foreground ,zenbenq-green+4))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,zenbenq-cyan))))
   `(circe-my-message-face ((t (:foreground ,zenbenq-fg))))
   `(circe-fool-face ((t (:foreground ,zenbenq-red+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,zenbenq-red :weight bold))))
   `(circe-originator-face ((t (:foreground ,zenbenq-fg))))
   `(circe-server-face ((t (:foreground ,zenbenq-green))))
   `(circe-topic-diff-new-face ((t (:foreground ,zenbenq-orange :weight bold))))
   `(circe-prompt-face ((t (:foreground ,zenbenq-orange :background ,zenbenq-bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,zenbenq-fg)))
   `(context-coloring-level-1-face ((t :foreground ,zenbenq-cyan)))
   `(context-coloring-level-2-face ((t :foreground ,zenbenq-green+4)))
   `(context-coloring-level-3-face ((t :foreground ,zenbenq-yellow)))
   `(context-coloring-level-4-face ((t :foreground ,zenbenq-orange)))
   `(context-coloring-level-5-face ((t :foreground ,zenbenq-magenta)))
   `(context-coloring-level-6-face ((t :foreground ,zenbenq-blue+1)))
   `(context-coloring-level-7-face ((t :foreground ,zenbenq-green+2)))
   `(context-coloring-level-8-face ((t :foreground ,zenbenq-yellow-2)))
   `(context-coloring-level-9-face ((t :foreground ,zenbenq-red+1)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,zenbenq-blue :foreground ,zenbenq-bg))))
   `(ctbl:face-continue-bar ((t (:background ,zenbenq-bg-05 :foreground ,zenbenq-bg))))
   `(ctbl:face-row-select ((t (:background ,zenbenq-cyan :foreground ,zenbenq-bg))))
;;;;; debbugs
   `(debbugs-gnu-done ((t (:foreground ,zenbenq-fg-1))))
   `(debbugs-gnu-handled ((t (:foreground ,zenbenq-green))))
   `(debbugs-gnu-new ((t (:foreground ,zenbenq-red))))
   `(debbugs-gnu-pending ((t (:foreground ,zenbenq-blue))))
   `(debbugs-gnu-stale ((t (:foreground ,zenbenq-orange))))
   `(debbugs-gnu-tagged ((t (:foreground ,zenbenq-red))))
;;;;; diff
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(diff-added          ((t (:background "#335533" :foreground ,zenbenq-green))))
   `(diff-changed        ((t (:background "#555511" :foreground ,zenbenq-yellow-1))))
   `(diff-removed        ((t (:background "#553333" :foreground ,zenbenq-red-2))))
   `(diff-refine-added   ((t (:background "#338833" :foreground ,zenbenq-green+4))))
   `(diff-refine-changed ((t (:background "#888811" :foreground ,zenbenq-yellow))))
   `(diff-refine-removed ((t (:background "#883333" :foreground ,zenbenq-red))))
   `(diff-header ((,class (:background ,zenbenq-bg+2))
                  (t (:background ,zenbenq-fg :foreground ,zenbenq-bg))))
   `(diff-file-header
     ((,class (:background ,zenbenq-bg+2 :foreground ,zenbenq-fg :weight bold))
      (t (:background ,zenbenq-fg :foreground ,zenbenq-bg :weight bold))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,zenbenq-blue :background ,zenbenq-blue-2))))
   `(diff-hl-delete ((,class (:foreground ,zenbenq-red+1 :background ,zenbenq-red-1))))
   `(diff-hl-insert ((,class (:foreground ,zenbenq-green+1 :background ,zenbenq-green-2))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,zenbenq-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,zenbenq-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,zenbenq-orange))))
   `(diredp-date-time ((t (:foreground ,zenbenq-magenta))))
   `(diredp-deletion ((t (:foreground ,zenbenq-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,zenbenq-red))))
   `(diredp-dir-heading ((t (:foreground ,zenbenq-blue :background ,zenbenq-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,zenbenq-cyan))))
   `(diredp-exec-priv ((t (:foreground ,zenbenq-red))))
   `(diredp-executable-tag ((t (:foreground ,zenbenq-green+1))))
   `(diredp-file-name ((t (:foreground ,zenbenq-blue))))
   `(diredp-file-suffix ((t (:foreground ,zenbenq-green))))
   `(diredp-flag-mark ((t (:foreground ,zenbenq-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,zenbenq-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,zenbenq-red))))
   `(diredp-link-priv ((t (:foreground ,zenbenq-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,zenbenq-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,zenbenq-orange))))
   `(diredp-no-priv ((t (:foreground ,zenbenq-fg))))
   `(diredp-number ((t (:foreground ,zenbenq-green+1))))
   `(diredp-other-priv ((t (:foreground ,zenbenq-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,zenbenq-red-1))))
   `(diredp-read-priv ((t (:foreground ,zenbenq-green-2))))
   `(diredp-symlink ((t (:foreground ,zenbenq-yellow))))
   `(diredp-write-priv ((t (:foreground ,zenbenq-magenta))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,zenbenq-red :weight bold))))
   `(dired-async-message ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,zenbenq-yellow))))
;;;;; diredfl
   `(diredfl-compressed-file-suffix ((t (:foreground ,zenbenq-orange))))
   `(diredfl-date-time ((t (:foreground ,zenbenq-magenta))))
   `(diredfl-deletion ((t (:foreground ,zenbenq-yellow))))
   `(diredfl-deletion-file-name ((t (:foreground ,zenbenq-red))))
   `(diredfl-dir-heading ((t (:foreground ,zenbenq-blue :background ,zenbenq-bg-1))))
   `(diredfl-dir-priv ((t (:foreground ,zenbenq-cyan))))
   `(diredfl-exec-priv ((t (:foreground ,zenbenq-red))))
   `(diredfl-executable-tag ((t (:foreground ,zenbenq-green+1))))
   `(diredfl-file-name ((t (:foreground ,zenbenq-blue))))
   `(diredfl-file-suffix ((t (:foreground ,zenbenq-green))))
   `(diredfl-flag-mark ((t (:foreground ,zenbenq-yellow))))
   `(diredfl-flag-mark-line ((t (:foreground ,zenbenq-orange))))
   `(diredfl-ignored-file-name ((t (:foreground ,zenbenq-red))))
   `(diredfl-link-priv ((t (:foreground ,zenbenq-yellow))))
   `(diredfl-no-priv ((t (:foreground ,zenbenq-fg))))
   `(diredfl-number ((t (:foreground ,zenbenq-green+1))))
   `(diredfl-other-priv ((t (:foreground ,zenbenq-yellow-1))))
   `(diredfl-rare-priv ((t (:foreground ,zenbenq-red-1))))
   `(diredfl-read-priv ((t (:foreground ,zenbenq-green-1))))
   `(diredfl-symlink ((t (:foreground ,zenbenq-yellow))))
   `(diredfl-write-priv ((t (:foreground ,zenbenq-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,zenbenq-fg :background ,zenbenq-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,zenbenq-fg :background ,zenbenq-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,zenbenq-fg :background ,zenbenq-green-2))))
   `(ediff-current-diff-C ((t (:foreground ,zenbenq-fg :background ,zenbenq-blue-5))))
   `(ediff-even-diff-A ((t (:background ,zenbenq-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,zenbenq-bg+1))))
   `(ediff-even-diff-B ((t (:background ,zenbenq-bg+1))))
   `(ediff-even-diff-C ((t (:background ,zenbenq-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,zenbenq-fg :background ,zenbenq-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,zenbenq-fg :background ,zenbenq-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,zenbenq-fg :background ,zenbenq-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,zenbenq-fg :background ,zenbenq-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,zenbenq-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,zenbenq-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,zenbenq-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,zenbenq-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,zenbenq-fg))))
   `(egg-help-header-1 ((t (:foreground ,zenbenq-yellow))))
   `(egg-help-header-2 ((t (:foreground ,zenbenq-green+3))))
   `(egg-branch ((t (:foreground ,zenbenq-yellow))))
   `(egg-branch-mono ((t (:foreground ,zenbenq-yellow))))
   `(egg-term ((t (:foreground ,zenbenq-yellow))))
   `(egg-diff-add ((t (:foreground ,zenbenq-green+4))))
   `(egg-diff-del ((t (:foreground ,zenbenq-red+1))))
   `(egg-diff-file-header ((t (:foreground ,zenbenq-yellow-2))))
   `(egg-section-title ((t (:foreground ,zenbenq-yellow))))
   `(egg-stash-mono ((t (:foreground ,zenbenq-green+4))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,zenbenq-red))))
   `(elfeed-log-info-level-face ((t (:foreground ,zenbenq-blue))))
   `(elfeed-log-warn-level-face ((t (:foreground ,zenbenq-yellow))))
   `(elfeed-search-date-face ((t (:foreground ,zenbenq-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,zenbenq-green))))
   `(elfeed-search-feed-face ((t (:foreground ,zenbenq-cyan))))
   `(elfeed-search-title-face ((t (:foreground ,zenbenq-fg-05))))
   `(elfeed-search-unread-title-face ((t (:foreground ,zenbenq-fg :weight bold))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,zenbenq-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,zenbenq-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,zenbenq-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,zenbenq-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,zenbenq-green+2 :background ,zenbenq-bg))))
   `(w3m-lnum-match ((t (:background ,zenbenq-bg-1
                                     :foreground ,zenbenq-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,zenbenq-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,zenbenq-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,zenbenq-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,zenbenq-yellow))))
   `(erc-keyword-face ((t (:foreground ,zenbenq-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,zenbenq-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,zenbenq-green))))
   `(erc-pal-face ((t (:foreground ,zenbenq-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,zenbenq-orange :background ,zenbenq-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,zenbenq-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; eros
   `(eros-result-overlay-face ((t (:background unspecified))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,zenbenq-green+4 :background ,zenbenq-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,zenbenq-red :background ,zenbenq-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,zenbenq-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,zenbenq-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,zenbenq-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,zenbenq-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,zenbenq-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,zenbenq-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenbenq-red-1) :inherit unspecified))
      (t (:foreground ,zenbenq-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenbenq-yellow) :inherit unspecified))
      (t (:foreground ,zenbenq-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenbenq-cyan) :inherit unspecified))
      (t (:foreground ,zenbenq-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,zenbenq-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,zenbenq-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenbenq-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenbenq-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenbenq-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenbenq-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenbenq-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenbenq-green-2 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenbenq-orange) :inherit unspecified))
      (t (:foreground ,zenbenq-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenbenq-red) :inherit unspecified))
      (t (:foreground ,zenbenq-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,zenbenq-fg))))
   `(ack-file ((t (:foreground ,zenbenq-blue))))
   `(ack-line ((t (:foreground ,zenbenq-yellow))))
   `(ack-match ((t (:foreground ,zenbenq-orange :background ,zenbenq-bg-1 :weight bold))))
;;;;; git-annex
   '(git-annex-dired-annexed-available ((t (:inherit success :weight normal))))
   '(git-annex-dired-annexed-unavailable ((t (:inherit error :weight normal))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,zenbenq-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,zenbenq-blue+1  :weight bold)))) ; obsolete
   `(git-commit-comment-branch-local  ((,class (:foreground ,zenbenq-blue+1  :weight bold))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,zenbenq-green  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,zenbenq-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,zenbenq-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,zenbenq-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,zenbenq-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,zenbenq-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,zenbenq-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,zenbenq-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,zenbenq-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, zenbenq-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:weight bold :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:weight bold :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:weight bold :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:weight bold :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:weight bold :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:weight bold :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:weight bold :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:weight bold :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:weight bold :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:weight bold :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:weight bold :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:weight bold :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:weight bold :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:weight bold :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,zenbenq-green+2 :weight bold))))
   `(gnus-server-denied ((t (:foreground ,zenbenq-red+1 :weight bold))))
   `(gnus-server-closed ((t (:foreground ,zenbenq-blue :slant italic))))
   `(gnus-server-offline ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(gnus-server-agent ((t (:foreground ,zenbenq-blue :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,zenbenq-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,zenbenq-blue))))
   `(gnus-summary-high-read ((t (:foreground ,zenbenq-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,zenbenq-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,zenbenq-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,zenbenq-blue))))
   `(gnus-summary-low-read ((t (:foreground ,zenbenq-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,zenbenq-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,zenbenq-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,zenbenq-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,zenbenq-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,zenbenq-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,zenbenq-fg))))
   `(gnus-summary-selected ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,zenbenq-blue))))
   `(gnus-cite-10 ((t (:foreground ,zenbenq-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,zenbenq-yellow))))
   `(gnus-cite-2 ((t (:foreground ,zenbenq-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,zenbenq-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,zenbenq-green+2))))
   `(gnus-cite-5 ((t (:foreground ,zenbenq-green+1))))
   `(gnus-cite-6 ((t (:foreground ,zenbenq-green))))
   `(gnus-cite-7 ((t (:foreground ,zenbenq-red))))
   `(gnus-cite-8 ((t (:foreground ,zenbenq-red-1))))
   `(gnus-cite-9 ((t (:foreground ,zenbenq-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,zenbenq-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,zenbenq-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,zenbenq-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,zenbenq-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,zenbenq-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,zenbenq-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,zenbenq-bg+2))))
   `(gnus-signature ((t (:foreground ,zenbenq-yellow))))
   `(gnus-x ((t (:background ,zenbenq-fg :foreground ,zenbenq-bg))))
   `(mm-uu-extract ((t (:background ,zenbenq-bg-05 :foreground ,zenbenq-green+1))))
;;;;; go-guru
   `(go-guru-hl-identifier-face ((t (:foreground ,zenbenq-bg-1 :background ,zenbenq-green+1))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,zenbenq-blue))))
   `(guide-key/key-face ((t (:foreground ,zenbenq-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,zenbenq-green+1))))
;;;;; hackernews
   '(hackernews-comment-count ((t (:inherit link-visited :underline nil))))
   '(hackernews-link          ((t (:inherit link         :underline nil))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,zenbenq-green
                      :background ,zenbenq-bg
                      :underline nil
                      :box nil
                      :extend t))))
   `(helm-source-header
     ((t (:foreground ,zenbenq-yellow
                      :background ,zenbenq-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)
                      :extend t))))
   `(helm-selection ((t (:background ,zenbenq-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,zenbenq-bg+1))))
   `(helm-visible-mark ((t (:foreground ,zenbenq-bg :background ,zenbenq-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,zenbenq-green+4 :background ,zenbenq-bg-1))))
   `(helm-separator ((t (:foreground ,zenbenq-red :background ,zenbenq-bg))))
   `(helm-time-zone-current ((t (:foreground ,zenbenq-green+2 :background ,zenbenq-bg))))
   `(helm-time-zone-home ((t (:foreground ,zenbenq-red :background ,zenbenq-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,zenbenq-orange :background ,zenbenq-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,zenbenq-magenta :background ,zenbenq-bg))))
   `(helm-bookmark-info ((t (:foreground ,zenbenq-green+2 :background ,zenbenq-bg))))
   `(helm-bookmark-man ((t (:foreground ,zenbenq-yellow :background ,zenbenq-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,zenbenq-magenta :background ,zenbenq-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,zenbenq-red :background ,zenbenq-bg))))
   `(helm-buffer-process ((t (:foreground ,zenbenq-cyan :background ,zenbenq-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,zenbenq-fg :background ,zenbenq-bg))))
   `(helm-buffer-size ((t (:foreground ,zenbenq-fg-1 :background ,zenbenq-bg))))
   `(helm-ff-directory ((t (:foreground ,zenbenq-cyan :background ,zenbenq-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,zenbenq-fg :background ,zenbenq-bg :weight normal))))
   `(helm-ff-file-extension ((t (:foreground ,zenbenq-fg :background ,zenbenq-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,zenbenq-green+2 :background ,zenbenq-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,zenbenq-red :background ,zenbenq-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,zenbenq-yellow :background ,zenbenq-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,zenbenq-bg :background ,zenbenq-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,zenbenq-cyan :background ,zenbenq-bg))))
   `(helm-grep-file ((t (:foreground ,zenbenq-fg :background ,zenbenq-bg))))
   `(helm-grep-finish ((t (:foreground ,zenbenq-green+2 :background ,zenbenq-bg))))
   `(helm-grep-lineno ((t (:foreground ,zenbenq-fg-1 :background ,zenbenq-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,zenbenq-red :background ,zenbenq-bg))))
   `(helm-match ((t (:foreground ,zenbenq-orange :background ,zenbenq-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,zenbenq-cyan :background ,zenbenq-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,zenbenq-fg-1 :background ,zenbenq-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,zenbenq-fg :background ,zenbenq-bg))))
;;;;; helm-lxc
   `(helm-lxc-face-frozen ((t (:foreground ,zenbenq-blue :background ,zenbenq-bg))))
   `(helm-lxc-face-running ((t (:foreground ,zenbenq-green :background ,zenbenq-bg))))
   `(helm-lxc-face-stopped ((t (:foreground ,zenbenq-red :background ,zenbenq-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,zenbenq-fg :background ,zenbenq-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,zenbenq-yellow :background ,zenbenq-bg+2 :weight bold))))
;;;;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,zenbenq-blue))))
;;;;; highlight-symbol
   `(highlight-symbol-face ((t (:background ,zenbenq-bg+2))))
;;;;; highlight-thing
   `(highlight-thing ((t (:background ,zenbenq-bg+2))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,zenbenq-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,zenbenq-bg-05 :extend t)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,zenbenq-bg+1))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,zenbenq-red-1 :background ,zenbenq-bg))))
   `(hydra-face-amaranth ((t (:foreground ,zenbenq-red-3 :background ,zenbenq-bg))))
   `(hydra-face-blue ((t (:foreground ,zenbenq-blue :background ,zenbenq-bg))))
   `(hydra-face-pink ((t (:foreground ,zenbenq-magenta :background ,zenbenq-bg))))
   `(hydra-face-teal ((t (:foreground ,zenbenq-cyan :background ,zenbenq-bg))))
;;;;; info+
   `(info-command-ref-item ((t (:background ,zenbenq-bg-1 :foreground ,zenbenq-orange))))
   `(info-constant-ref-item ((t (:background ,zenbenq-bg-1 :foreground ,zenbenq-magenta))))
   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
   `(info-file ((t (:background ,zenbenq-bg-1 :foreground ,zenbenq-yellow))))
   `(info-function-ref-item ((t (:background ,zenbenq-bg-1 :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((t (:background ,zenbenq-bg-1 :foreground ,zenbenq-yellow))))
   `(info-menu ((t (:foreground ,zenbenq-yellow))))
   `(info-quoted-name ((t (:inherit font-lock-constant-face))))
   `(info-reference-item ((t (:background ,zenbenq-bg-1))))
   `(info-single-quote ((t (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((t (:background ,zenbenq-bg-1 :foreground ,zenbenq-yellow))))
   `(info-string ((t (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((t (:background ,zenbenq-bg-1 :foreground ,zenbenq-blue+1))))
   `(info-user-option-ref-item ((t (:background ,zenbenq-bg-1 :foreground ,zenbenq-red))))
   `(info-variable-ref-item ((t (:background ,zenbenq-bg-1 :foreground ,zenbenq-orange))))
;;;;; irfc
   `(irfc-head-name-face ((t (:foreground ,zenbenq-red :weight bold))))
   `(irfc-head-number-face ((t (:foreground ,zenbenq-red :weight bold))))
   `(irfc-reference-face ((t (:foreground ,zenbenq-blue-1 :weight bold))))
   `(irfc-requirement-keyword-face ((t (:inherit font-lock-keyword-face))))
   `(irfc-rfc-link-face ((t (:inherit link))))
   `(irfc-rfc-number-face ((t (:foreground ,zenbenq-cyan :weight bold))))
   `(irfc-std-number-face ((t (:foreground ,zenbenq-green+4 :weight bold))))
   `(irfc-table-item-face ((t (:foreground ,zenbenq-green+3))))
   `(irfc-title-face ((t (:foreground ,zenbenq-yellow
                                      :underline t :weight bold))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,zenbenq-green :background ,zenbenq-bg))))
   `(ivy-current-match ((t (:foreground ,zenbenq-yellow :weight bold :underline t))))
   `(ivy-cursor ((t (:foreground ,zenbenq-bg :background ,zenbenq-fg))))
   `(ivy-match-required-face ((t (:foreground ,zenbenq-red :background ,zenbenq-bg))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,zenbenq-bg+1))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,zenbenq-green-2))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,zenbenq-green))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,zenbenq-green+1))))
   `(ivy-remote ((t (:foreground ,zenbenq-blue :background ,zenbenq-bg))))
   `(ivy-subdir ((t (:foreground ,zenbenq-yellow :background ,zenbenq-bg))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,zenbenq-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,zenbenq-yellow))))
   `(ido-indicator ((t (:foreground ,zenbenq-yellow :background ,zenbenq-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,zenbenq-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,zenbenq-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,zenbenq-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,zenbenq-red+1))))
   `(jabber-roster-user-xa ((t (:foreground ,zenbenq-magenta))))
   `(jabber-roster-user-chatty ((t (:foreground ,zenbenq-orange))))
   `(jabber-roster-user-error ((t (:foreground ,zenbenq-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,zenbenq-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,zenbenq-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,zenbenq-red+1))))
   `(jabber-chat-prompt-system ((t (:foreground ,zenbenq-green+3))))
   `(jabber-activity-face((t (:foreground ,zenbenq-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,zenbenq-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,zenbenq-orange))))
   `(js2-error ((t (:foreground ,zenbenq-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,zenbenq-green-2))))
   `(js2-jsdoc-type ((t (:foreground ,zenbenq-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,zenbenq-green+3))))
   `(js2-function-param ((t (:foreground, zenbenq-orange))))
   `(js2-external-variable ((t (:foreground ,zenbenq-orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,zenbenq-green-2))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,zenbenq-orange))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,zenbenq-red-1))))
   `(js2-object-property ((t (:foreground ,zenbenq-blue+1))))
   `(js2-magic-paren ((t (:foreground ,zenbenq-blue-5))))
   `(js2-private-function-call ((t (:foreground ,zenbenq-cyan))))
   `(js2-function-call ((t (:foreground ,zenbenq-cyan))))
   `(js2-private-member ((t (:foreground ,zenbenq-blue-1))))
   `(js2-keywords ((t (:foreground ,zenbenq-magenta))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,zenbenq-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,zenbenq-fg :weight normal))))
   `(ledger-font-payee-pending-face ((t (:foreground ,zenbenq-red :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,zenbenq-bg+1))))
   `(ledger-font-auto-xact-face ((t (:foreground ,zenbenq-yellow-1 :weight normal))))
   `(ledger-font-periodic-xact-face ((t (:foreground ,zenbenq-green :weight normal))))
   `(ledger-font-pending-face ((t (:foreground ,zenbenq-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,zenbenq-fg))))
   `(ledger-font-posting-date-face ((t (:foreground ,zenbenq-orange :weight normal))))
   `(ledger-font-posting-account-face ((t (:foreground ,zenbenq-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,zenbenq-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,zenbenq-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,zenbenq-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,zenbenq-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,zenbenq-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,zenbenq-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,zenbenq-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,zenbenq-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,zenbenq-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,zenbenq-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,zenbenq-green+2 :background ,zenbenq-bg))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,zenbenq-bg-05 :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,zenbenq-bg :background ,zenbenq-fg))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,zenbenq-yellow))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,zenbenq-fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,zenbenq-yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,zenbenq-yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,zenbenq-green+2 :background ,zenbenq-bg))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,zenbenq-blue-1))))
   `(lui-hilight-face ((t (:foreground ,zenbenq-green+2 :background ,zenbenq-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,zenbenq-green+2 :background ,zenbenq-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,zenbenq-red+1 :background ,zenbenq-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,zenbenq-blue+1 :background ,zenbenq-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,zenbenq-magenta :background ,zenbenq-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,zenbenq-yellow :background ,zenbenq-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(magit-section-highlight           ((t (:background ,zenbenq-bg+05))))
   `(magit-section-heading             ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,zenbenq-orange :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,zenbenq-bg+05 :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,zenbenq-bg+05 :weight bold
                                                        :foreground ,zenbenq-orange))))
   `(magit-diff-added                  ((t (:background ,zenbenq-green-2))))
   `(magit-diff-added-highlight        ((t (:background ,zenbenq-green))))
   `(magit-diff-removed                ((t (:background ,zenbenq-red-4))))
   `(magit-diff-removed-highlight      ((t (:background ,zenbenq-red-3))))
   `(magit-diff-hunk-heading           ((t (:background ,zenbenq-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,zenbenq-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,zenbenq-bg+2
                                                        :foreground ,zenbenq-orange))))
   `(magit-diff-lines-heading          ((t (:background ,zenbenq-orange
                                                        :foreground ,zenbenq-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,zenbenq-bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added              ((t (:foreground ,zenbenq-green+4))))
   `(magit-diffstat-removed            ((t (:foreground ,zenbenq-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,zenbenq-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,zenbenq-green-2 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,zenbenq-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,zenbenq-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,zenbenq-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,zenbenq-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,zenbenq-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,zenbenq-orange))))
   `(magit-log-date      ((t (:foreground ,zenbenq-fg-1))))
   `(magit-log-graph     ((t (:foreground ,zenbenq-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,zenbenq-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,zenbenq-green))))
   `(magit-sequence-part ((t (:foreground ,zenbenq-yellow))))
   `(magit-sequence-head ((t (:foreground ,zenbenq-blue))))
   `(magit-sequence-drop ((t (:foreground ,zenbenq-red))))
   `(magit-sequence-done ((t (:foreground ,zenbenq-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,zenbenq-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,zenbenq-green))))
   `(magit-bisect-skip ((t (:foreground ,zenbenq-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,zenbenq-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,zenbenq-bg-1 :foreground ,zenbenq-blue-2))))
   `(magit-blame-hash    ((t (:background ,zenbenq-bg-1 :foreground ,zenbenq-blue-2))))
   `(magit-blame-name    ((t (:background ,zenbenq-bg-1 :foreground ,zenbenq-orange))))
   `(magit-blame-date    ((t (:background ,zenbenq-bg-1 :foreground ,zenbenq-orange))))
   `(magit-blame-summary ((t (:background ,zenbenq-bg-1 :foreground ,zenbenq-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,zenbenq-bg+3))))
   `(magit-hash           ((t (:foreground ,zenbenq-bg+3))))
   `(magit-tag            ((t (:foreground ,zenbenq-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,zenbenq-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,zenbenq-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,zenbenq-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,zenbenq-blue   :weight bold))))
   `(magit-refname        ((t (:background ,zenbenq-bg+2 :foreground ,zenbenq-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,zenbenq-bg+2 :foreground ,zenbenq-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,zenbenq-bg+2 :foreground ,zenbenq-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,zenbenq-green))))
   `(magit-signature-bad       ((t (:foreground ,zenbenq-red))))
   `(magit-signature-untrusted ((t (:foreground ,zenbenq-yellow))))
   `(magit-signature-expired   ((t (:foreground ,zenbenq-orange))))
   `(magit-signature-revoked   ((t (:foreground ,zenbenq-magenta))))
   '(magit-signature-error     ((t (:inherit    magit-signature-bad))))
   `(magit-cherry-unmatched    ((t (:foreground ,zenbenq-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,zenbenq-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,zenbenq-green))))
   `(magit-reflog-amend        ((t (:foreground ,zenbenq-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,zenbenq-green))))
   `(magit-reflog-checkout     ((t (:foreground ,zenbenq-blue))))
   `(magit-reflog-reset        ((t (:foreground ,zenbenq-red))))
   `(magit-reflog-rebase       ((t (:foreground ,zenbenq-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,zenbenq-green))))
   `(magit-reflog-remote       ((t (:foreground ,zenbenq-cyan))))
   `(magit-reflog-other        ((t (:foreground ,zenbenq-cyan))))
;;;;; markup-faces
   `(markup-anchor-face ((t (:foreground ,zenbenq-blue+1))))
   `(markup-code-face ((t (:inherit font-lock-constant-face))))
   `(markup-command-face ((t (:foreground ,zenbenq-yellow))))
   `(markup-emphasis-face ((t (:inherit bold))))
   `(markup-internal-reference-face ((t (:foreground ,zenbenq-yellow-2 :underline t))))
   `(markup-list-face ((t (:foreground ,zenbenq-fg+1))))
   `(markup-meta-face ((t (:foreground ,zenbenq-yellow))))
   `(markup-meta-hide-face ((t (:foreground ,zenbenq-yellow))))
   `(markup-secondary-text-face ((t (:foreground ,zenbenq-yellow-1))))
   `(markup-title-0-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-1-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-2-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-3-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-4-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-typewriter-face ((t (:inherit font-lock-constant-face))))
   `(markup-verbatim-face ((t (:inherit font-lock-constant-face))))
   `(markup-value-face ((t (:foreground ,zenbenq-yellow))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,zenbenq-green+1))))
   `(message-header-other ((t (:foreground ,zenbenq-green))))
   `(message-header-to ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,zenbenq-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,zenbenq-green))))
   `(message-mml ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,zenbenq-orange))))
   `(mew-face-header-from ((t (:foreground ,zenbenq-yellow))))
   `(mew-face-header-date ((t (:foreground ,zenbenq-green))))
   `(mew-face-header-to ((t (:foreground ,zenbenq-red))))
   `(mew-face-header-key ((t (:foreground ,zenbenq-green))))
   `(mew-face-header-private ((t (:foreground ,zenbenq-green))))
   `(mew-face-header-important ((t (:foreground ,zenbenq-blue))))
   `(mew-face-header-marginal ((t (:foreground ,zenbenq-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,zenbenq-red))))
   `(mew-face-header-xmew ((t (:foreground ,zenbenq-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,zenbenq-red))))
   `(mew-face-body-url ((t (:foreground ,zenbenq-orange))))
   `(mew-face-body-comment ((t (:foreground ,zenbenq-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,zenbenq-green))))
   `(mew-face-body-cite2 ((t (:foreground ,zenbenq-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,zenbenq-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,zenbenq-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,zenbenq-red))))
   `(mew-face-mark-review ((t (:foreground ,zenbenq-blue))))
   `(mew-face-mark-escape ((t (:foreground ,zenbenq-green))))
   `(mew-face-mark-delete ((t (:foreground ,zenbenq-red))))
   `(mew-face-mark-unlink ((t (:foreground ,zenbenq-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,zenbenq-green))))
   `(mew-face-mark-unread ((t (:foreground ,zenbenq-red-2))))
   `(mew-face-eof-message ((t (:foreground ,zenbenq-green))))
   `(mew-face-eof-part ((t (:foreground ,zenbenq-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,zenbenq-cyan :background ,zenbenq-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,zenbenq-bg :background ,zenbenq-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,zenbenq-bg :background ,zenbenq-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,zenbenq-blue))))
   `(mingus-pausing-face ((t (:foreground ,zenbenq-magenta))))
   `(mingus-playing-face ((t (:foreground ,zenbenq-cyan))))
   `(mingus-playlist-face ((t (:foreground ,zenbenq-cyan ))))
   `(mingus-mark-face ((t (:bold t :foreground ,zenbenq-magenta))))
   `(mingus-song-file-face ((t (:foreground ,zenbenq-yellow))))
   `(mingus-artist-face ((t (:foreground ,zenbenq-cyan))))
   `(mingus-album-face ((t (:underline t :foreground ,zenbenq-red+1))))
   `(mingus-album-stale-face ((t (:foreground ,zenbenq-red+1))))
   `(mingus-stopped-face ((t (:foreground ,zenbenq-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,zenbenq-yellow))))
   `(nav-face-button-num ((t (:foreground ,zenbenq-cyan))))
   `(nav-face-dir ((t (:foreground ,zenbenq-green))))
   `(nav-face-hdir ((t (:foreground ,zenbenq-red))))
   `(nav-face-file ((t (:foreground ,zenbenq-fg))))
   `(nav-face-hfile ((t (:foreground ,zenbenq-red-4))))
;;;;; merlin
   `(merlin-type-face ((t (:inherit highlight))))
   `(merlin-compilation-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenbenq-orange)))
      (t
       (:underline ,zenbenq-orange))))
   `(merlin-compilation-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenbenq-red)))
      (t
       (:underline ,zenbenq-red))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,zenbenq-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,zenbenq-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,zenbenq-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,zenbenq-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,zenbenq-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,zenbenq-green-2 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,zenbenq-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,zenbenq-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,zenbenq-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,zenbenq-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,zenbenq-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,zenbenq-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,zenbenq-bg+1))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,zenbenq-blue+1 :weight bold))))
   `(neo-header-face ((t (:foreground ,zenbenq-fg))))
   `(neo-root-dir-face ((t (:foreground ,zenbenq-blue+1 :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,zenbenq-blue))))
   `(neo-file-link-face ((t (:foreground ,zenbenq-fg))))
   `(neo-expand-btn-face ((t (:foreground ,zenbenq-blue))))
   `(neo-vc-default-face ((t (:foreground ,zenbenq-fg+1))))
   `(neo-vc-user-face ((t (:foreground ,zenbenq-red :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,zenbenq-fg))))
   `(neo-vc-edited-face ((t (:foreground ,zenbenq-magenta))))
   `(neo-vc-needs-merge-face ((t (:foreground ,zenbenq-red+1))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,zenbenq-red :background ,zenbenq-blue-5))))
   `(neo-vc-added-face ((t (:foreground ,zenbenq-green+1))))
   `(neo-vc-conflict-face ((t (:foreground ,zenbenq-red+1))))
   `(neo-vc-missing-face ((t (:foreground ,zenbenq-red+1))))
   `(neo-vc-ignored-face ((t (:foreground ,zenbenq-fg-1))))
;;;;; notmuch
   `(notmuch-crypto-decryption ((t (:foreground ,zenbenq-bg :background ,zenbenq-magenta))))
   `(notmuch-crypto-part-header ((t (:foreground ,zenbenq-blue+1))))
   `(notmuch-crypto-signature-bad ((t (:foreground ,zenbenq-bg :background ,zenbenq-red))))
   `(notmuch-crypto-signature-good ((t (:foreground ,zenbenq-bg :background ,zenbenq-green+1))))
   `(notmuch-crypto-signature-good-key ((t (:foreground ,zenbenq-bg :background ,zenbenq-orange))))
   `(notmuch-crypto-signature-unknown ((t (:foreground ,zenbenq-bg :background ,zenbenq-red))))
   `(notmuch-hello-logo-background ((t (:background ,zenbenq-bg+2))))
   `(notmuch-message-summary-face ((t (:background ,zenbenq-bg-08))))
   `(notmuch-search-flagged-face ((t (:foreground ,zenbenq-blue+1))))
   `(notmuch-search-non-matching-authors ((t (:foreground ,zenbenq-fg-1))))
   `(notmuch-tag-added ((t (:underline ,zenbenq-green+1))))
   `(notmuch-tag-deleted ((t (:strike-through ,zenbenq-red))))
   `(notmuch-tag-face ((t (:foreground ,zenbenq-green+1))))
   `(notmuch-tag-flagged ((t (:foreground ,zenbenq-blue+1))))
   `(notmuch-tag-unread ((t (:foreground ,zenbenq-red))))
   `(notmuch-tree-match-author-face ((t (:foreground ,zenbenq-green+1))))
   `(notmuch-tree-match-tag-face ((t (:foreground ,zenbenq-green+1))))
;;;;; orderless
   `(orderless-match-face-0 ((t (:foreground ,zenbenq-green))))
   `(orderless-match-face-1 ((t (:foreground ,zenbenq-magenta))))
   `(orderless-match-face-2 ((t (:foreground ,zenbenq-blue))))
   `(orderless-match-face-3 ((t (:foreground ,zenbenq-orange))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,zenbenq-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,zenbenq-fg :weight bold))))
   `(org-block ((t (:background ,zenbenq-bg+05 :extend t))))
   `(org-checkbox ((t (:background ,zenbenq-bg+2 :foreground ,zenbenq-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,zenbenq-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,zenbenq-red-1))))
   `(org-done ((t (:weight bold :weight bold :foreground ,zenbenq-green+3))))
   `(org-formula ((t (:foreground ,zenbenq-yellow-2))))
   `(org-headline-done ((t (:foreground ,zenbenq-green+3))))
   `(org-hide ((t (:foreground ,zenbenq-bg))))
   `(org-level-1 ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-orange
                               ,@(when zenbenq-scale-org-headlines
                                   (list :height zenbenq-height-plus-4))))))
   `(org-level-2 ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-green+4
                               ,@(when zenbenq-scale-org-headlines
                                   (list :height zenbenq-height-plus-3))))))
   `(org-level-3 ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-blue-1
                               ,@(when zenbenq-scale-org-headlines
                                   (list :height zenbenq-height-plus-2))))))
   `(org-level-4 ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-yellow-2
                               ,@(when zenbenq-scale-org-headlines
                                   (list :height zenbenq-height-plus-1))))))
   `(org-level-5 ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-cyan))))
   `(org-level-6 ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-green+2))))
   `(org-level-7 ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-red-4))))
   `(org-level-8 ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-blue-4))))
   `(org-link ((t (:foreground ,zenbenq-yellow-2 :underline t))))
   `(org-quote ((t (:background ,zenbenq-bg+05 :extend t))))
   `(org-scheduled ((t (:foreground ,zenbenq-green+4))))
   `(org-scheduled-previously ((t (:foreground ,zenbenq-red))))
   `(org-scheduled-today ((t (:foreground ,zenbenq-blue+1))))
   `(org-sexp-date ((t (:foreground ,zenbenq-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,zenbenq-green+2))))
   `(org-tag ((t (:weight bold :weight bold))))
   `(org-time-grid ((t (:foreground ,zenbenq-orange))))
   `(org-todo ((t (:weight bold :foreground ,zenbenq-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:weight bold :foreground ,zenbenq-red :weight bold :underline nil))))
   `(org-column ((t (:background ,zenbenq-bg-1))))
   `(org-column-title ((t (:background ,zenbenq-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,zenbenq-fg :background ,zenbenq-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,zenbenq-bg :background ,zenbenq-red-1))))
   `(org-ellipsis ((t (:foreground ,zenbenq-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,zenbenq-cyan :underline t))))
   `(org-document-title ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-blue
                                      :weight bold
                                      ,@(when zenbenq-scale-org-headlines
                                          (list :height zenbenq-height-plus-4))))))
   `(org-document-info ((t (:foreground ,zenbenq-blue))))
   `(org-habit-ready-face ((t :background ,zenbenq-green)))
   `(org-habit-alert-face ((t :background ,zenbenq-yellow-1 :foreground ,zenbenq-bg)))
   `(org-habit-clear-face ((t :background ,zenbenq-blue-3)))
   `(org-habit-overdue-face ((t :background ,zenbenq-red-3)))
   `(org-habit-clear-future-face ((t :background ,zenbenq-blue-4)))
   `(org-habit-ready-future-face ((t :background ,zenbenq-green-2)))
   `(org-habit-alert-future-face ((t :background ,zenbenq-yellow-2 :foreground ,zenbenq-bg)))
   `(org-habit-overdue-future-face ((t :background ,zenbenq-red-4)))
;;;;; org-ref
   `(org-ref-ref-face ((t :underline t)))
   `(org-ref-label-face ((t :underline t)))
   `(org-ref-cite-face ((t :underline t)))
   `(org-ref-glossary-face ((t :underline t)))
   `(org-ref-acronym-face ((t :underline t)))
;;;;; outline
   `(outline-1 ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-orange
                             ,@(when zenbenq-scale-outline-headlines
                                 (list :height zenbenq-height-plus-4))))))
   `(outline-2 ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-green+4
                             ,@(when zenbenq-scale-outline-headlines
                                 (list :height zenbenq-height-plus-3))))))
   `(outline-3 ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-blue-1
                             ,@(when zenbenq-scale-outline-headlines
                                 (list :height zenbenq-height-plus-2))))))
   `(outline-4 ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-yellow-2
                             ,@(when zenbenq-scale-outline-headlines
                                 (list :height zenbenq-height-plus-1))))))
   `(outline-5 ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-cyan))))
   `(outline-6 ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-green+2))))
   `(outline-7 ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-red-4))))
   `(outline-8 ((t (:inherit ,z-variable-pitch :foreground ,zenbenq-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; c/perl
   `(cperl-nonoverridable-face ((t (:foreground ,zenbenq-magenta))))
   `(cperl-array-face ((t (:foreground ,zenbenq-yellow, :backgorund ,zenbenq-bg))))
   `(cperl-hash-face ((t (:foreground ,zenbenq-yellow-1, :background ,zenbenq-bg))))
;;;;; paren-face
   `(parenthesis ((t (:foreground ,zenbenq-fg-1))))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,zenbenq-yellow-2))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,zenbenq-fg :background ,zenbenq-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,zenbenq-bg :background ,zenbenq-orange))))
   `(proof-error-face ((t (:foreground ,zenbenq-fg :background ,zenbenq-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,zenbenq-bg :background ,zenbenq-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,zenbenq-bg :background ,zenbenq-orange))))
   `(proof-locked-face ((t (:background ,zenbenq-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,zenbenq-bg :background ,zenbenq-orange))))
   `(proof-queue-face ((t (:background ,zenbenq-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,zenbenq-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,zenbenq-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,zenbenq-bg))))
   `(proof-warning-face ((t (:foreground ,zenbenq-bg :background ,zenbenq-yellow-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))

;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,zenbenq-blue))))
   `(rcirc-other-nick ((t (:foreground ,zenbenq-orange))))
   `(rcirc-bright-nick ((t (:foreground ,zenbenq-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,zenbenq-blue-2))))
   `(rcirc-server ((t (:foreground ,zenbenq-green))))
   `(rcirc-server-prefix ((t (:foreground ,zenbenq-green+1))))
   `(rcirc-timestamp ((t (:foreground ,zenbenq-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,zenbenq-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:weight bold))))
   `(rcirc-prompt ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:weight bold))))
   `(rcirc-url ((t (:weight bold))))
   `(rcirc-keyword ((t (:foreground ,zenbenq-yellow :weight bold))))
;;;;; re-builder
   `(reb-match-0 ((t (:foreground ,zenbenq-bg :background ,zenbenq-magenta))))
   `(reb-match-1 ((t (:foreground ,zenbenq-bg :background ,zenbenq-blue))))
   `(reb-match-2 ((t (:foreground ,zenbenq-bg :background ,zenbenq-orange))))
   `(reb-match-3 ((t (:foreground ,zenbenq-bg :background ,zenbenq-red))))
;;;;; realgud
   `(realgud-overlay-arrow1 ((t (:foreground ,zenbenq-green))))
   `(realgud-overlay-arrow2 ((t (:foreground ,zenbenq-yellow))))
   `(realgud-overlay-arrow3 ((t (:foreground ,zenbenq-orange))))
   `(realgud-bp-enabled-face ((t (:inherit error))))
   `(realgud-bp-disabled-face ((t (:inherit secondary-selection))))
   `(realgud-bp-line-enabled-face ((t (:box (:color ,zenbenq-red :style nil)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color "grey70" :style nil)))))
   `(realgud-line-number ((t (:foreground ,zenbenq-yellow))))
   `(realgud-backtrace-number ((t (:foreground ,zenbenq-yellow, :weight bold))))
;;;;; regex-tool
   `(regex-tool-matched-face ((t (:background ,zenbenq-blue-4 :weight bold))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,zenbenq-green))))
   `(rpm-spec-doc-face ((t (:foreground ,zenbenq-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,zenbenq-red))))
   `(rpm-spec-macro-face ((t (:foreground ,zenbenq-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,zenbenq-red))))
   `(rpm-spec-package-face ((t (:foreground ,zenbenq-red))))
   `(rpm-spec-section-face ((t (:foreground ,zenbenq-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,zenbenq-blue))))
   `(rpm-spec-var-face ((t (:foreground ,zenbenq-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,zenbenq-orange))))
   `(rst-level-2-face ((t (:foreground ,zenbenq-green+1))))
   `(rst-level-3-face ((t (:foreground ,zenbenq-blue-1))))
   `(rst-level-4-face ((t (:foreground ,zenbenq-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,zenbenq-cyan))))
   `(rst-level-6-face ((t (:foreground ,zenbenq-green-2))))
;;;;; selectrum
   `(selectrum-current-candidate ((t (:foreground ,zenbenq-yellow :weight bold :underline t))))
   `(selectrum-primary-highlight ((t (:background ,zenbenq-green-2))))
   `(selectrum-secondary-highlight ((t (:background ,zenbenq-green))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(sh-quoted-exec ((t (:foreground ,zenbenq-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,zenbenq-red+1 :background ,zenbenq-bg+3 :weight bold))))
   `(show-paren-match ((t (:foreground ,zenbenq-fg :background ,zenbenq-bg+3 :weight bold))))

;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,zenbenq-red+1 :background ,zenbenq-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,zenbenq-bg+3 :weight bold))))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,zenbenq-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,zenbenq-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenbenq-red)))
      (t
       (:underline ,zenbenq-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenbenq-orange)))
      (t
       (:underline ,zenbenq-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenbenq-yellow)))
      (t
       (:underline ,zenbenq-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenbenq-green)))
      (t
       (:underline ,zenbenq-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; solaire
   `(solaire-default-face ((t (:inherit default :background ,zenbenq-bg-08))))
   `(solaire-minibuffer-face ((t (:inherit default :background ,zenbenq-bg-08))))
   `(solaire-hl-line-face ((t (:inherit hl-line :background ,zenbenq-bg))))
   `(solaire-org-hide-face ((t (:inherit org-hide :background ,zenbenq-bg-08))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,zenbenq-green+2))))
   `(speedbar-directory-face ((t (:foreground ,zenbenq-cyan))))
   `(speedbar-file-face ((t (:foreground ,zenbenq-fg))))
   `(speedbar-highlight-face ((t (:foreground ,zenbenq-bg :background ,zenbenq-green+2))))
   `(speedbar-selected-face ((t (:foreground ,zenbenq-red))))
   `(speedbar-separator-face ((t (:foreground ,zenbenq-bg :background ,zenbenq-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,zenbenq-yellow))))
;;;;; swiper
   `(swiper-line-face ((t (:underline t))))
;;;;; sx
   `(sx-custom-button
     ((t (:background ,zenbenq-fg :foreground ,zenbenq-bg-1
                      :box (:line-width 3 :style released-button) :height 0.9))))
   `(sx-question-list-answers
     ((t (:foreground ,zenbenq-green+3
                      :height 1.0 :inherit sx-question-list-parent))))
   `(sx-question-mode-accepted
     ((t (:foreground ,zenbenq-green+3
                      :height 1.3 :inherit sx-question-mode-title))))
   '(sx-question-mode-content-face ((t (:inherit highlight))))
   `(sx-question-mode-kbd-tag
     ((t (:box (:color ,zenbenq-bg-1 :line-width 3 :style released-button)
               :height 0.9 :weight semi-bold))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,zenbenq-fg
                                    :background ,zenbenq-bg))))
   `(tabbar-selected ((t (:foreground ,zenbenq-fg
                                      :background ,zenbenq-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,zenbenq-fg
                                        :background ,zenbenq-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,zenbenq-bg
                                       :background ,zenbenq-bg-1))))
   `(term-color-red ((t (:foreground ,zenbenq-red-2
                                     :background ,zenbenq-red-4))))
   `(term-color-green ((t (:foreground ,zenbenq-green
                                       :background ,zenbenq-green+2))))
   `(term-color-yellow ((t (:foreground ,zenbenq-orange
                                        :background ,zenbenq-yellow))))
   `(term-color-blue ((t (:foreground ,zenbenq-blue-1
                                      :background ,zenbenq-blue-4))))
   `(term-color-magenta ((t (:foreground ,zenbenq-magenta
                                         :background ,zenbenq-red))))
   `(term-color-cyan ((t (:foreground ,zenbenq-cyan
                                      :background ,zenbenq-blue))))
   `(term-color-white ((t (:foreground ,zenbenq-fg
                                       :background ,zenbenq-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,zenbenq-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,zenbenq-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,zenbenq-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,zenbenq-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,zenbenq-cyan))))
;;;;; vertico
   `(vertico-current ((t (:foreground ,zenbenq-yellow :weight bold :underline t))))
;;;;; visual-regexp
   `(vr/group-0 ((t (:foreground ,zenbenq-bg :background ,zenbenq-green :weight bold))))
   `(vr/group-1 ((t (:foreground ,zenbenq-bg :background ,zenbenq-orange :weight bold))))
   `(vr/group-2 ((t (:foreground ,zenbenq-bg :background ,zenbenq-blue :weight bold))))
   `(vr/match-0 ((t (:inherit isearch))))
   `(vr/match-1 ((t (:foreground ,zenbenq-yellow-2 :background ,zenbenq-bg-1 :weight bold))))
   `(vr/match-separator-face ((t (:foreground ,zenbenq-red :weight bold))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,zenbenq-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,zenbenq-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,zenbenq-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,zenbenq-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,zenbenq-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,zenbenq-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,zenbenq-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,zenbenq-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,zenbenq-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,zenbenq-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,zenbenq-bg+1 :foreground ,zenbenq-bg+1))))
   `(whitespace-hspace ((t (:background ,zenbenq-bg+1 :foreground ,zenbenq-bg+1))))
   `(whitespace-tab ((t (:background ,zenbenq-red-1))))
   `(whitespace-newline ((t (:foreground ,zenbenq-bg+1))))
   `(whitespace-trailing ((t (:background ,zenbenq-red))))
   `(whitespace-line ((t (:background ,zenbenq-bg :foreground ,zenbenq-magenta))))
   `(whitespace-space-before-tab ((t (:background ,zenbenq-orange :foreground ,zenbenq-orange))))
   `(whitespace-indentation ((t (:background ,zenbenq-yellow :foreground ,zenbenq-red))))
   `(whitespace-empty ((t (:background ,zenbenq-yellow))))
   `(whitespace-space-after-tab ((t (:background ,zenbenq-yellow :foreground ,zenbenq-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,zenbenq-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,zenbenq-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,zenbenq-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,zenbenq-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,zenbenq-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,zenbenq-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,zenbenq-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,zenbenq-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,zenbenq-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,zenbenq-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,zenbenq-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,zenbenq-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,zenbenq-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,zenbenq-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,zenbenq-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,zenbenq-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,zenbenq-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,zenbenq-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,zenbenq-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,zenbenq-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,zenbenq-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,zenbenq-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,zenbenq-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,zenbenq-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,zenbenq-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,zenbenq-green+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,zenbenq-yellow :weight bold))))
   `(cscope-function-face ((t (:foreground ,zenbenq-cyan :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,zenbenq-red :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,zenbenq-bg :background ,zenbenq-blue+1))))
   `(cscope-separator-face ((t (:foreground ,zenbenq-red :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,zenbenq-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,zenbenq-bg-1 :foreground ,zenbenq-bg-1))))
   ))

;;; Theme Variables
(zenbenq-with-color-variables
  (custom-theme-set-variables
   'zenbenq
;;;;; ansi-color
   `(ansi-color-names-vector [,zenbenq-bg ,zenbenq-red ,zenbenq-green ,zenbenq-yellow
                                          ,zenbenq-blue ,zenbenq-magenta ,zenbenq-cyan ,zenbenq-fg])
;;;;; company-quickhelp
   `(company-quickhelp-color-background ,zenbenq-bg+1)
   `(company-quickhelp-color-foreground ,zenbenq-fg)
;;;;; fill-column-indicator
   `(fci-rule-color ,zenbenq-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,zenbenq-red ,zenbenq-orange ,zenbenq-yellow ,zenbenq-green ,zenbenq-green+4
       ,zenbenq-cyan ,zenbenq-blue+1 ,zenbenq-magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,zenbenq-fg . ,zenbenq-bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,zenbenq-red-1)
       ( 40. . ,zenbenq-red)
       ( 60. . ,zenbenq-orange)
       ( 80. . ,zenbenq-yellow-2)
       (100. . ,zenbenq-yellow-1)
       (120. . ,zenbenq-yellow)
       (140. . ,zenbenq-green-2)
       (160. . ,zenbenq-green)
       (180. . ,zenbenq-green+1)
       (200. . ,zenbenq-green+2)
       (220. . ,zenbenq-green+3)
       (240. . ,zenbenq-green+4)
       (260. . ,zenbenq-cyan)
       (280. . ,zenbenq-blue-2)
       (300. . ,zenbenq-blue-1)
       (320. . ,zenbenq-blue)
       (340. . ,zenbenq-blue+1)
       (360. . ,zenbenq-magenta)))
   `(vc-annotate-very-old-color ,zenbenq-magenta)
   `(vc-annotate-background ,zenbenq-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defcustom zenbenq-add-font-lock-keywords nil
  "Whether to add font-lock keywords for zenbenq color names.

In buffers visiting library `zenbenq-theme.el' the zenbenq
specific keywords are always added, provided that library has
been loaded (because that is where the code that does it is
defined).  If you visit this file and only enable the theme,
then you have to turn `rainbow-mode' off and on again for the
zenbenq-specific font-lock keywords to be used.

In all other Emacs-Lisp buffers this variable controls whether
this should be done.  This requires library `rainbow-mode'."
  :type 'boolean
  :group 'zenbenq-theme)

(defvar zenbenq-colors-font-lock-keywords nil)

(defun zenbenq--rainbow-turn-on ()
  "Maybe also add font-lock keywords for zenbenq colors."
  (when (and (derived-mode-p 'emacs-lisp-mode)
             (or zenbenq-add-font-lock-keywords
                 (and (buffer-file-name)
                      (equal (file-name-nondirectory (buffer-file-name))
                             "zenbenq-theme.el"))))
    (unless zenbenq-colors-font-lock-keywords
      (setq zenbenq-colors-font-lock-keywords
            `((,(regexp-opt (mapcar 'car zenbenq-default-colors-alist) 'words)
               (0 (rainbow-colorize-by-assoc zenbenq-default-colors-alist))))))
    (font-lock-add-keywords nil zenbenq-colors-font-lock-keywords 'end)))

(defun zenbenq--rainbow-turn-off ()
  "Also remove font-lock keywords for zenbenq colors."
  (font-lock-remove-keywords nil zenbenq-colors-font-lock-keywords))

(when (fboundp 'advice-add)
  (advice-add 'rainbow-turn-on :after  #'zenbenq--rainbow-turn-on)
  (advice-add 'rainbow-turn-off :after #'zenbenq--rainbow-turn-off))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'zenbenq)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; zenbenq-theme.el ends here
