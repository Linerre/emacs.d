;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Domain Specific Languages such as Nix, SQL, Tex, Markdown etc.

;; Disable capf to avoid `/' character triggering capf errors
(defun +verilog-company-backends ()
  (setq-local company-backends
              '(company-semantic
                company-keywords
                company-dabbrev-code
                company-gtags
                ;; company-files
                company-capf)))

(add-to-list 'auto-mode-alist '("\\.s?vh?\\'" . verilog-ts-mode))

(setq fpga-feature-list '(xilinx))
;; verilog-ts-mode requires verilog-mode which sets
;; indentation to 3 by default.  This indentation affects <Return>
;; for newline-and-indent functionality.  Solution is either to
;; set `verilog-indent-level' to 4 or `verilog-ts-indent-level' to
;; 3 (defaults to 4)
(setq verilog-ts-indent-level 3
      verilog-auto-newline nil)

(with-eval-after-load "verilog-ts-mode"
  (define-key verilog-ts-mode-map "\C-c\C-i" #'verilog-pretty-declarations)
  (require 'fpga))

(provide 'lang-verilog)
;;; lang-verilog.el ends here
