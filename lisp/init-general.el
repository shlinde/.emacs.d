;;; init-general.el --- Prepare the General -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

;;; Summon the General
(use-package general
  :ensure t
  :demand t
  :config
  ;;; Create Definers
  (general-create-definer general-spc
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC"))

(provide 'init-general)
;;; init-general.el ends here
