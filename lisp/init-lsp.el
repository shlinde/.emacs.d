;;; init-lsp.el --- LSP Setup -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

;; (require 'init-evil)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom
  ;; Performance
  (read-process-output-max (* 1024 1024))
  (lsp-idle-delay 0.500)
  (lsp-log-io nil)

  ;; General settings
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-folding t)
  (lsp-enable-snippet t)
  (lsp-enable-file-watchers nil)

  :config
  ;; (evil-define-key 'normal lsp-mode-map
  ;;   "gd" 'lsp-find-definition
  ;;   "gr" 'lsp-find-references
  ;;   ;; "gi" 'lsp-find-implementation
  ;;   ;; "gt" 'lsp-find-type-definition
  ;;   "K" 'lsp-describe-thing-at-point)

  (add-hook 'before-save-hook #'lsp-format-buffer nil t))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  ;; Sideline configuration
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover t)

  ;; Doc configuration
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-max-width 120)

  ;; Peek configuration
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t))

  ;; :config
  ;; (evil-define-key 'normal lsp-ui-mode-map
  ;;   "gh" 'lsp-ui-doc-show
  ;;   "gl" 'lsp-ui-peek-find-references
  ;;   "gL" 'lsp-ui-peek-find-definitions))


(provide 'init-lsp)
;;; init-lsp.el ends here+
