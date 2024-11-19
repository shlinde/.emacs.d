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

(use-package cc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (when (listp c-default-style)
    (setf (alist-get 'other c-default-style) "llvm")))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

;;; IEdit: rename the symbol under point
(use-package iedit
  :ensure t
  :init
  ;;; Fix A bug (normal key is "C-;")1
  :bind ("C-c ;" . #'iedit-mode))

(use-package cmake-mode
  :ensure t)

(provide 'shl-langs)
;;; shl-langs.el ends here
