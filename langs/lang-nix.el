;;; -*- lexical-binding: t -*-

(straight-use-package 'nix-mode)

(autoload #'nix-mode "nix-mode")

(with-eval-after-load "nix-mode"
  (add-hook 'nix-mode-hook 'smartparens-mode))

(provide 'lang-nix)
