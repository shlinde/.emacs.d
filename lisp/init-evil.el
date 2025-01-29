;;; init.el --- Custom configurations -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

(require 'init-general)

;; Store more undo history to prevent loss of data
(setq undo-limit 8000000
      undo-strong-limit 8000000
      undo-outer-limit 8000000)

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
(general-spc "a" 'aider-transient-menu)

;; 
(general-spc "ff" #'find-file)

;; Buffers
(general-spc "bb" #'consult-buffer)
(general-spc "bv" #'consult-buffer-other-window)
(general-spc "bk" #'kill-current-buffer)

;; Project
(general-spc "p!" #'project-shell-command)
(general-spc "p&" #'project-async-shell-command)
(general-spc "pD" #'project-dired)
(general-spc "pF" #'project-or-external-find-file)
(general-spc "pG" #'project-or-external-find-file)
(general-spc "pG" #'project-or-external-find-regexp)
(general-spc "pb" #'consult-project-buffer)
(general-spc "pc" #'project-compile)
(general-spc "pd" #'project-find-dir)
(general-spc "pe" #'project-eshell)
(general-spc "pf" #'project-find-file)
(general-spc "pg" #'project-find-regexp)
(general-spc "pk" #'project-kill-buffers)
(general-spc "po" #'project-any-command)
(general-spc "pp" #'project-switch-project)
(general-spc "pr" #'project-query-replace-regexp)
(general-spc "ps" #'project-shell)
(general-spc "pv" #'project-vc-dir)
(general-spc "px" #'project-execute-extended-command)

;; Git
(general-spc "gg" #'magit-status)
(general-spc "gs" #'magit-stage-file)
(general-spc "gc" #'magit-commit)

;; Evaluate
(general-spc "er" #'eval-region)

;; Org / Org-Roam
(general-spc "nl" #'org-roam-buffer-toggle)
(general-spc "nf" #'org-roam-node-find)
(general-spc "ng" #'org-roam-graph)
(general-spc "ni" #'org-roam-node-insert)
(general-spc "nc" #'org-roam-capture)
(general-spc "nj" #'org-roam-dailies-capture-today)


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
    "ø" #'evil-goto-mark-line)
  (evil-define-key '(normal visual) 'global "gc" #'evilnc-comment-operator))

(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :ensure t
  :general ([remap comment-line] #'evilnc-comment-or-uncomment-lines))

(provide 'init-evil)
;;; init-evil.el ends here
