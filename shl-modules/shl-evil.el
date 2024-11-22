;;; Evil Configuration -*- lexical-binding: t -*-
;;; Commentary
;;;    This module is responsible for ensuring Evil is set up properly
;;; Code:

(use-package undo-fu
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setopt evil-want-keybinding nil
          evil-want-integration t)

  :config
  (evil-mode 1)

  (setq undo-limit 8000000
        undo-strong-limit 8000000
        undo-outer-limit 8000000)

  ;; set leader key in normal state
  (evil-set-leader 'normal (kbd "SPC"))

  ;; set local leader
  (evil-set-leader 'normal "," t)

  (evil-define-key 'normal 'global (kbd "<leader>ff") 'project-find-file)

  ;; How to handle undo
  (setq evil-undo-system 'undo-redo)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo)
  (define-key evil-normal-state-map "U" 'undo-fu-only-redo)

  (setq evil-symbol-word-search t)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-default-cursor t))

(use-package evil-surround
  :ensure t
  :defer 2
  :config
  (global-evil-surround-mode)
  (defun evil-surround-prog-mode-hook-setup ()
    "Set up surround shortcuts."
    (cond
     ((memq major-mode '(sh-mode))
      (push '(?$ . ("$(" . ")")) evil-surround-pairs-alist))
     (t
      (push '(?$ . ("${" . "}")) evil-surround-pairs-alist)))

    (when (memq major-mode '(org-mode))
      (push '(?\[ . ("[[" . "]]")) evil-surround-pairs-alist) ; [
      (push '(?= . ("=" . "=")) evil-surround-pairs-alist))

    (when (memq major-mode '(emacs-lisp-mode))
      (push '(?\( . ("( " . ")")) evil-surround-pairs-alist)
      (push '(?` . ("`" . "'")) evil-surround-pairs-alist))

    (when (or (derived-mode-p 'js-mode)
              (memq major-mode '(typescript-mode web-mode)))
      (push '(?j . ("JSON.stringify(" . ")")) evil-surround-pairs-alist)
      (push '(?> . ("(e) => " . "(e)")) evil-surround-pairs-alist))

    ;; generic
    (push '(?/ . ("/" . "/")) evil-surround-pairs-alist))
  (add-hook 'prog-mode-hook 'evil-surround-prog-mode-hook-setup))

;; Comments
(use-package evil-commentary
  :ensure t
  :after evil
  :config (evil-commentary-mode))

(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init))


