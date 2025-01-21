;;; init.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;; Use `emacs --debug-init' when noticing any problems
;;; Code:

;;; Alias
(defalias 'sup 'straight-use-package "Another alias for straight-use-package.")

;;; Consts

(require 'init-packages)
;;; Load the modular configs
(require 'init-options)
(require 'init-utils)
(require 'init-gui)
(require 'init-meow)
(require 'init-completion)
(require 'init-paredit)
(require 'init-git)
(require 'lang-lisps)
(require 'lang-fp)
(require 'lang-rust)
(require 'lang-cpp)
(require 'lang-go)
(require 'lang-script)
(require 'lang-web)
(require 'lang-dsl)

;;; init.el ends here
