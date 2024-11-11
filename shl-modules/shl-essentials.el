;;; Essential Configurations for Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;;     This module sets up some essential configurations for Emacs.
;;;     It ensures that the Emacs PATH and system PATH is aligned.
;;;     Removes Emacs cluttering capabilities.
;;;     Ensures that we save our interaction history.
;;; Code:

;; Setup PATH to follow system
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (or (memq window-system '(mac ns x pgtk))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
    (exec-path-from-shell-initialize)))

(use-package files
  :ensure nil
  :config
  (let ((backup-dir "~/.cache/tmp/emacs/backups")
        (auto-saves-dir "~/.cache/tmp/emacs/auto-saves/"))
    (dolist (dir (list backup-dir auto-saves-dir))
      (when (not (file-directory-p dir))
        (make-directory dir t)))
    (setq backup-directory-alist `(("." . ,backup-dir))
          auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
          tramp-backup-directory-alist `((".*" . ,backup-dir))
          tramp-auto-save-directory auto-saves-dir))
  (setopt backup-by-copying t    ; Don't delink hardlinks
	  delete-old-versions t  ; Clean up the backups
	  version-control t      ; Use version numbers on backups,
	  kept-new-versions 5    ; keep some new versions
	  kept-old-versions 2))   ; and some old ones, too

;; Turn on savehist mode
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))

;; 
(use-package emacs
  :ensure nil
  :config
  (setopt custom-file (concat user-emacs-directory "custom.el")
          bookmark-default-file (locate-user-emacs-file ".bookmarks.el")  ;; Hide bookmarks.el, to not clutter user-emacs-dir
          inhibit-splash-screen t
	  case-fold-search t  ;; Ignore case while searching
	  create-lockfiles nil  ;; Don't clutter directories with lock files
	  save-interprogram-paste-before-kill t  ;; Save existing clipboard text into kill ring before replacing.
          apropos-do-all t
          load-prefer-newer t))


(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

;; Ensure that opening parentheses are paired with closing
(use-package elec-pair
  :ensure nil
  :hook ((prog-mode . electric-pair-mode)))

(use-package electric
  :ensure nil
  :hook ((prog-mode . electric-indent-mode)))

(use-package delsel
  :ensure nil
  :hook ((after-init . delete-selection-mode)))

(provide 'shl-essentials)
;;; shl-essentials.el ends here
