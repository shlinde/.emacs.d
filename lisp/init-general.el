;;; init-general.el --- General Emacs setup -*- lexical-binding: t; -*-
;;; Code:

;;; Use el-patch for patching function
(use-package el-patch
  :ensure t
  :demand t)

;;; General
(use-package general
  :ensure (:wait t)
  :demand t
  :config

  ;; Leader (global override map)
  (general-create-definer shl/leader
    :keymaps 'override
    :prefix "C-c"
    :global-prefix "C-c")

  ;; Local leader (per major mode if you want later)
  (general-create-definer shl/local-leader
    :prefix "C-c m")) 

(provide 'init-general)
;;; init-general.el ends here
