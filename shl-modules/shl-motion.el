;;; Configuration of Tools Aiding in Motion -*- lexical-binding: t -*-
;;; Commentary:
;;;    All the following tools aid with motion in some form or another:
;;;    - Subword: Moving around CamelCase and snake_case.
;;;    - Expand Region: Iteratively highlighting text
;;;    - Isearch: Motion by search
;;;    - Vertico: Minibuffer Completion
;;;    - Consult: Minibuffer Interface
;;;    - Marginalia: Minibuffer Information
;;;    - Embark: Minibuffer Actions
;;; Code:

;; Isearch
(use-package isearch
  :ensure nil
  :bind (:map global-map
              ("C-s" . isearch-forward-regexp)
              ("C-r" . isearch-backward-regexp)
              ("C-M-s" . isearch-forward)
              ("C-M-r" . isearch-backward)))

;; Expand Region makes for a nicer way to mark stuff
(use-package expand-region
  :ensure t
  :bind (("M-h" . er/expand-region)))

;; Subword-mode enables moving in CamelCase and snake_case
(use-package subword
  :ensure nil
  :hook ((after-init . global-subword-mode)))

;; Vertico - Vertical Interactive Completion
(use-package vertico
  :ensure t
  :hook ((after-init . vertico-mode))
  :config
  (setopt vertico-scroll-margin 0
	  vertico-count 5
	  vertico-resize t
	  vertico-cycle t))

;; Embark - Minibuffer Actions
(use-package embark
  :ensure t
  :bind (:map global-map
              ("C-." . embark-act)
              ("M-." . embark-dwim)))

;; Consulting completing-read
(use-package consult
  :ensure t
  :bind (:map global-map
              ("C-x b" . consult-buffer)
              ("M-g g" . consult-goto-line)
              :map org-mode-map
              ("M-g g" . consult-org-heading)))
  
;; Glue package
(use-package embark-consult
  :ensure t
  :after embark
  :hook ((embar-collect-mode . embark-collect-preview-minor-mode)))

;; Marginalia in the minibuffer
(use-package marginalia
  :ensure t
  :hook ((after-init . marginalia-mode))
  :config
  (setopt marginalia-max-relative-age 0)) ;; Use absolute time

;; Orderless for better search and motions in search
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

(provide 'shl-motion)
;;; shl-motion.el ends here

