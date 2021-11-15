;;;  -*- lexical-binding: t -*-

;;; Commentary:
;;; Modularized initialization
;;; Author: Errelin
;;; Last Change: Sat Aug 21 23:41:36 2021
;;; Modules stored in the "elisp" directory

;;; Code:

(require 'init-utils)
(require 'init-theme)
(require 'init-meow)
(require 'init-completion)
<<<<<<< HEAD
;(require 'init-lsp)
;(require 'init-clojure)
;(require 'init-lisp)
;(require 'init-python)
;(require 'init-javascript)
;(require 'init-nix)
;(require 'init-markdown)
;(require 'init-web)
(require 'init-git)
(require 'init-modeline-dog)
;(require 'init-org)
;(require 'init-latex)
;(require 'init-server)
=======
(require 'init-smartparens)
(require 'init-git)
(require 'init-modeline-dog)
(require 'lang-lisp)
(require 'lang-nix)
(require 'lang-web)
>>>>>>> js/vue-mode
