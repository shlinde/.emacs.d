;;; init-dired.el --- Configuration of Dired Behavior -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

(use-package dirvish
  :ensure t
  :bind (("C-c r" . dirvish))
  :config
  (setq dirvish-header-line-height '(25 . 35)
        dirvish-mode-line-height 25)

  (dirvish-override-dired-mode)
  (dirvish-peek-mode))

(provide 'init-dired)
;;; init-dired.el ends here
