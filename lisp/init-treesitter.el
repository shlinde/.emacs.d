;;; init-treesitter.el --- Treesitter Configuration -*- lexical-binding: t; -*-
;;; Code:

;;; Treesitter
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
