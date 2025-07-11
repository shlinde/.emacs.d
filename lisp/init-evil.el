;;; init-evil.el --- Vim Movements -*- lexical-binding: t; -*-
;;; Code:


(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-create-definer shl/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (shl/leader-keys
    "b"  '(:ignore t :which-key "buffer")
    "bb"  #'consult-buffer
    "bk"  #'kill-current-buffer
    "t"  '(:ignore t :which-key "toggles")
    "f"  '(:ignore t :which-key "toggles")
    "ff" #'find-file
    "fr" #'consult-recent-file
    "fm" #'consult-man
    "fi" #'consult-info
    "h"  #'help-command
    "p"  '(:ignore t :which-key "project")
    "pb" #'consult-project-buffer
    "pf" #'project-find-file
    "pg" #'project-find-regexp
    "g" '(:ignore t :which-key "git")
    "gg" #'magit-status
    "w" '(:ignore t :which-key "workspaces")
    ))


(defun shl/evil-hook ()
  (dolist (mode '(custom-mode
		  dired-mode
		  eshell-mode
		  git-rebase-mode
		  erc-mode
		  circe-server-mode
		  circe-chat-mode
		  circe-query-mode
		  sauron-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :ensure t
  :hook ((elpaca-after-init . evil-mode)
	 (evil-mode . shl/evil-hook))
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-want-C-u-scroll t
	evil-want-C-i-jump nil)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


(use-package evil-collection
  :ensure t
  :after evil
  :config (evil-collection-init))

(provide 'init-evil)
;;; init-evil.el ends here
