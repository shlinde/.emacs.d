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
(savehist-mode 1)

;; Settings
(setopt ring-bell-function 'ignore
	custom-file (concat user-emacs-directory "custom.el")
        bookmark-default-file (locate-user-emacs-file ".bookmarks.el")  ;; Hide bookmarks.el, to not clutter user-emacs-dir
        use-short-answers t  ;; Use y and n instead of yes and no.
	buffer-menu-max-size 30
        inhibit-splash-screen t
	case-fold-search t  ;; Ignore case while searching
	column-number-mode t  ;; Show column number in modeline
	indent-tabs-mode nil  ;; Ensure that all indentation is with spaces
	create-lockfiles nil  ;; Don't clutter directories with lock files
	save-interprogram-paste-before-kill t  ;; Save existing clipboard text into kill ring before replacing.
	scroll-preserve-screen-position 'always  ;; Ensure that scrolling does not move point
        truncate-lines nil ;; Truncate lines when wider than buffer-width
        truncate-partial-width-windows nil
        save-interprogram-paste-before-kill t
        apropos-do-all t
        require-final-newline t
        load-prefer-newer t)

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install))

;; Ensure that opening parentheses are paired with closing
(use-package elec-pair
  :ensure nil
  :hook ((prog-mode . electric-pair-mode)))

(use-package electric
  :ensure nil
  :hook ((prog-mode . electric-indent-mode)))

;; Subword-mode enables moving in CamelCase and snake_case
(use-package subword
  :ensure nil
  :hook ((after-init . global-subword-mode)))


(use-package delsel
  :ensure nil
  :hook ((after-init . delete-selection-mode)))

;; Expand Region makes for a nicer way to mark stuff
(use-package expand-region
  :ensure t
  :bind (("M-h" . er/expand-region)))

(use-package hippie-exp
  :ensure nil
  :bind (:map global-map
         ("M-/" . hippie-expand)))

(use-package isearch
  :ensure nil
  :bind (:map global-map
              ("C-s" . isearch-forward-regexp)
              ("C-r" . isearch-backward-regexp)
              ("C-M-s" . isearch-forward)
              ("C-M-r" . isearch-backward)))

(use-package rfc-mode
  :ensure t
  :config
  (setq rfc-mode-directory (expand-file-name "~/data/resources/rfc/")))

(setq browse-url-function 'eww-browse
      shr-use-colors nil
      shr-folding-mode t)

(global-set-key (kbd "C-c w") 'eww)

(provide 'shl-essentials)
