;;; init.el --- Custom configurations -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

;; Needed for Magit to ensure correct version
(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(provide 'init-vcs)
;;; init-vcs.el ends here
