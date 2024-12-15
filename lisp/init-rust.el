;;; init-rust.el --- Rust Programming -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

;;; Zig Mode
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

(provide 'init-rust)
;;; init-rust.el ends here
