 ;;; init-core.el --- Core Settings and Helpers -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

(require 'init-core)

(defun shl-tree-sitter-enable ()
  "Dispatch to turn on tree-sitter"
  (turn-on-tree-sitter-mode)
  (tree-sitter-hl-mode)
  (ts-fold-mode))
  
(use-package tree-sitter
  :ensure t
  :defer t
  :init
  (setq-default treesit-font-lock-level 4)
  :config
  (setq tree-sitter-debug-jump-buttons t ; This makes every node a link to a section of code.
        tree-sitter-debug-highlight-jump-region t)) ; Highlight the entire sub tree in your code.

(use-package ts-fold
  :ensure (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :hook (after-init . global-ts-fold-mode))

(use-package tree-sitter-langs
  :ensure t)

(use-package tree-sitter-indent
  :ensure t)

(use-package evil-textobj-tree-sitter
  :if shl-evil-p
  :ensure t)

(provide 'init-treesitter)
;;; init-treesitter.el ends heref

