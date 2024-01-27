;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(with-eval-after-load "haskell-mode"
  (add-hook 'haskell-mode-hook #'flymake-hlint-load)
  (add-hook 'haskell-mode-hook #'hindent-mode))

(provide 'lang-haskell)
;;; lang-haskell.el ends here
