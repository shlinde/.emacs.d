;;; init-tools.el --- Tool Configuration -*- lexical-binding: t; -*-
;;; Code:

(use-package bluetooth
  :ensure t
  :bind ("C-c t b" . bluetooth-list-devices))

(provide 'init-tools)
;;; init-tools.el ends here
