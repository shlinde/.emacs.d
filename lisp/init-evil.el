;;; init.el --- Custom configurations -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

(require 'init-general)

;;; Evil Core
(use-package evil
  :ensure t
  :demand t
  :init
  (general-setq evil-overriding-maps nil
                evil-intercept-maps nil
                evil-insert-state-bindings nil
                evil-want-keybinding nil ; must be set before loading evil no matter what
                evil-search-module 'evil-search
                evil-ex-search-persistent-highlight nil
                evil-want-Y-yank-to-eol t)
  (evil-mode))


;;; Keybinds
(general-spc "ff" #'find-file)

(provide 'init-evil)
;;; init-evil.el ends here
