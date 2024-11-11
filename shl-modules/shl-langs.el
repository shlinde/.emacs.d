;;; Language Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;;     Configuration for the following languages:
;;;     - Python
;;;     - Zig
;;; Code:

;; Zig
(use-package zig-mode
  :ensure t)

;; Python
(use-package pet
  :ensure t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package pyvenv
  :ensure t)

(provide 'shl-langs)
;;; shl-langs.el ends here
