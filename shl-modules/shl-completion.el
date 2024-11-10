(use-package vertico
  :ensure t
  :hook ((after-init . vertico-mode))
  :config
  (setopt vertico-scroll-margin 0
	  vertico-count 5
	  vertico-resize t
	  vertico-cycle t))

(use-package embark
  :ensure t
  :bind (:map global-map
              ("C-." . embark-act)
              ("M-." . embark-dwim)))

(use-package consult
  :ensure t
  :bind (:map global-map
              ("C-x b" . consult-buffer)
              ("M-g g" . consult-goto-line)
              :map org-mode-map
              ("M-g g" . consult-org-heading)))
  
(use-package embark-consult
  :ensure t
  :after embark
  :hook ((embar-collect-mode . embark-collect-preview-minor-mode)))

(use-package marginalia
  :ensure t
  :hook ((after-init . marginalia-mode))
  :config
  (setopt marginalia-max-relative-age 0)) ;; Use absolute time

(use-package orderless
  :ensure t
  :config
  (setq completion-category-defaults nil
        completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless)))
        completion-cycle-threshold 4)

  (with-eval-after-load 'vertico
    (require 'orderless)
    (setq completion-styles '(orderless basic))))

(use-package corfu
  :ensure t
  :hook ((after-init . global-corfu-mode))
  :bind (:map corfu-map
              ("C-y" . corfu-insert)
              ("RET" . nil))
  :config
  (setq global-corfu-modes '((not erc-mode
                                  circe-mode
                                  help-mode
                                  gud-mode
                                  eat-mode
                                  inferior-python-mode)
                             t))

  (setopt corfu-auto t
	  corfu-auto-delay 0.1
	  corfu-auto-prefix 1
	  corfu-cycle t
	  corfu-separator ?\s
	  corfu-preselect 'first
	  corfu-count 16
	  corfu-max-width 120
	  corfu-preview-current nil
	  corfu-on-exact-match nil
	  corfu-quit-at-boundary 'separator
	  corfu-quit-no-match 'separator)
  (setq text-mode-ispell-word-completion nil))

(use-package corfu-history
  :after corfu
  :hook ((corfu-mode . corfu-history-mode))
  :config
  (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package corfu-popupinfo
  :after corfu
  :hook ((corfu-mode . corfu-popupinfo-mode))
  :config
  (setq corfu-popupinfo-delay '(0.5 . 1.0)))

(provide 'shl-completion)
