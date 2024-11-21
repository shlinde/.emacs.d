;;; Tool Configuration for Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;;     This module intends to set up most of the tools I use.
;;;     These include:
;;;     - Magit: Wonderful `git' frontend for Emacs
;;;     - Eshell: Emacs shell frontend
;;;     - Eat: Emulate a terminal
;;;     - Compilation: Emacs compilation command
;;;     - RFC: Read RFCs in Emacs
;;;     - EWW: Browse the web in Emacs
;;;     - PDF Tools: Read PDFs in Emacs
;;;     - Corfu: Completion in Region function
;;; Code:

;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setopt magit-repository-directories
          '(("~/data/source" . 1))))

;; Compilation buffer
(defun shl/colorize-compilation-buffer ()
  (read-only-mode 'toggle)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode 'toggle))
(add-hook 'compilation-filter-hook 'shl/colorize-compilation-buffer)

;; Eshell
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          #'(lambda()
              (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(shl-pkg-config 'esh-autosuggest
  (add-hook 'eshell-mode-hook #'esh-autosuggest-mode))

;; Eat
(use-package eat
  :ensure t
  :bind (("C-c t t" . eat)))

;; RFC reading
(use-package rfc-mode
  :ensure t
  :config
  (setq rfc-mode-directory (expand-file-name "~/data/resources/rfc/")))

;; EWW
(setq browse-url-function 'eww-browse
      shr-use-colors nil
      shr-folding-mode t)

(global-set-key (kbd "C-c w") 'eww)

;; PDF Tools
(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install))

;; Completion in Region
;; (use-package corfu
;;   :ensure t
;;   :hook ((after-init . global-corfu-mode))
;;   :bind (:map corfu-map
;;               ("C-y" . corfu-insert)
;;               ("RET" . nil))
;;   :config
;;   (setq global-corfu-modes '((not erc-mode
;;                                   circe-mode
;;                                   help-mode
;;                                   gud-mode
;;                                   eat-mode
;;                                   inferior-python-mode)
;;                              t))

;;   (setopt corfu-auto t
;; 	  corfu-auto-delay 0.1
;; 	  corfu-auto-prefix 1
;; 	  corfu-cycle t
;; 	  corfu-separator ?\s
;; 	  corfu-preselect 'first
;; 	  corfu-count 16
;; 	  corfu-max-width 120
;; 	  corfu-preview-current nil
;; 	  corfu-on-exact-match nil
;; 	  corfu-quit-at-boundary 'separator
;; 	  corfu-quit-no-match 'separator)
;;   (setq text-mode-ispell-word-completion nil))

;; (use-package corfu-history
;;   :after corfu
;;   :hook ((corfu-mode . corfu-history-mode))
;;   :config
;;   (with-eval-after-load 'savehist
;;       (add-to-list 'savehist-additional-variables 'corfu-history)))

;; (use-package corfu-popupinfo
;;   :after corfu
;;   :hook ((corfu-mode . corfu-popupinfo-mode))
;;   :config
;;   (setq corfu-popupinfo-delay '(0.5 . 1.0)))

;; Better Dabbrev 
(use-package hippie-exp
  :ensure nil
  :bind (:map global-map
         ("M-/" . hippie-expand)))

;; Treesitter
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-font-lock-level 4)
  (setq c-ts-mode-indent-offset 4
        c-ts-mode-indent-style 'linux)

  ;; (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc)
  :config
  ;; Don't follow the cursor (it's more disruptive/jarring than helpful as a default)
  (treemacs-follow-mode -1))


(use-package treemacs-nerd-icons
  :ensure t
  :defer t
  ;; HACK: Because `lsp-treemacs' mutates Treemacs' default theme, and
  ;;   `treemacs-nerd-icons' reads from it to populate its nerd-icons theme,
  ;;   load order is important to ensure they don't step on each other's toes.
  :config (treemacs-load-theme "nerd-icons"))

;; Direnv integration
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

(use-package rtags
  :ensure t)

;; CMake Integration
(use-package cmake-ide
  :ensure t
  :config
  (require 'rtags)
  (cmake-ide-setup))

;; LSP
(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable t
        lsp-ui-peek-enable t
        lsp-ui-peek-always-show t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "C-c C-l .") 'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map (kbd "C-c C-l ?") 'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "C-c C-l r") 'lsp-rename)
  (define-key lsp-ui-mode-map (kbd "C-c C-l x") 'lsp-workspace-restart)
  (define-key lsp-ui-mode-map (kbd "C-c C-l w") 'lsp-ui-peek-find-workspace-symbol)
  (define-key lsp-ui-mode-map (kbd "C-c C-l i") 'lsp-ui-peek-find-implementation)
  (define-key lsp-ui-mode-map (kbd "C-c C-l d") 'lsp-describe-thing-at-point)
  (define-key lsp-ui-mode-map (kbd "C-c C-l e") 'lsp-execute-code-action))
  

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setopt company-global-modes '(not erc-mode message-mode eat-mode inferior-python-mode)
          company-idle-delay 1
          company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.2))))


(provide 'shl-tools)
;;; shl-tools.el ends here
