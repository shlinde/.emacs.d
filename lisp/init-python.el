;;; init-python.el --- Python Settings and Helpers -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

(require 'init-treesitter)

;; Virtual Environments
(defun shl-python-hook ()
  "Activate virtual environment and start LSP."
  (shl-tree-sitter-enable)
  (let* ((venv_path (concat (locate-dominating-file "." "pyproject.toml") ".venv/")))
    (ignore-error
      (require 'pyvenv)
      (pyvenv-activate venv_path))
    (ignore-error
      (require 'lsp)
      (require 'lsp-pyright)
      (lsp))))

(add-hook 'python-mode-hook #'shl-python-hook)

(use-package python
  :config
  ;; Set check command
  (setopt python-check-command "ruff")
  (add-hook 'python-mode-hook #'flymake-mode))

(use-package pyvenv
  :ensure t
  :demand t)
  
(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "basedpyright")) ;; or basedpyright

(provide 'init-python)
;;; init-python.el ends here
