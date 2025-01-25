;;; init-zig.el --- Zig Programming -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

;;; Dependencies - maybe move to separate module
(use-package reformatter
  :ensure t)

;;; Zig Mode
(use-package zig-mode
  :ensure t)

(provide 'init-zig)
;;; init-zig.el ends here
