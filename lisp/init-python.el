;;; init-python.el --- Python Settings and Helpers -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

;; Virtual Environments
(defun shl-python-hook ()
  "Activate virtual environment and start LSP."
  (let* ((venv_path (concat (locate-dominating-file "." "pyproject.toml") ".venv/")))
    (with-eval-after-load 'pyvenv
      (pyvenv-activate venv_path))
    (with-eval-after-load 'lsp-mode
      (require 'lsp-pyright)
      (lsp))))

(use-package python
  :config
  (setopt python-check-command "ruff")
  (add-hook 'python-mode-hook #'flymake-mode))

(use-package pyvenv
  :ensure t
  :demand t
  :hook (python-base-mode . shl-python-hook))


(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "basedpyright")) ;; or basedpyright

(provide 'init-python)
;;; init-python.el ends here
