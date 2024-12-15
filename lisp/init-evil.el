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
                evil-want-Y-yank-to-eol t
                evil-want-fine-undo t)
  (evil-mode))


;;; Keybinds
(general-spc "ff" #'find-file)

(general-with 'evil
  ;; I always disliked this behavior in vim
  (general-setq evil-move-cursor-back nil
                ;; this doesn't matter as much with above setting
                evil-move-beyond-eol t
                ;; default to inserting `<,`> when run `evil-ex' in visual char state;
                ;; unlike vim, ex commands will only apply to the selected region instead
                ;; of the selected lines when `<,`> is used
                evil-ex-visual-char-range t))

;; not necessary to set `evil-normal-state-modes' (since normal is the default
;; state) but it's more explicit
(general-with 'evil
  (general-setq evil-normal-state-modes (append evil-emacs-state-modes
                                                evil-normal-state-modes)
                evil-emacs-state-modes nil
                evil-motion-state-modes nil))

(general-with 'evil
  (general-def 'normal
    "ø" #'evil-goto-mark-line))

(provide 'init-evil)
;;; init-evil.el ends here
