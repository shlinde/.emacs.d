;;; init-ai.el --- AI Setup -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:


;;; Completion
(use-package corfu
  :disabled
  :ensure (corfu :files (:defaults "extensions/*.el"))
  :hook (after-init . global-corfu-mode)
  :config
  (setopt corfu-auto t
          corfu-auto-delay 0.15
          corfu-auto-prefix 1)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :ensure t
  :init
  ;; company adapter examples
  (setq-local completion-at-point-functions
              (mapcar #'cape-company-to-capf
                      (list #'company-files
                            #'company-ispell
                            #'company-dabbrev)))
  (setq completion-at-point-functions
        (list
         (cape-company-to-capf
          (apply-partially #'company--multi-backend-adapter
                           '(company-dabbrev company-elisp))))))

;;; Templates
;; Configure Tempel
(use-package tempel
  :disabled
  :ensure t
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         :map tempel-map
         ("M-æ" . tempel-next)
         ("M-S-æ" . tempel-previous))
  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  (global-tempel-abbrev-mode))


;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection
  :disabled
  :ensure t)

;;; Company
(use-package company
  :ensure t
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  (company-selection-wrap-around t)
  (copmany-tooltip-align-annotations t)
  (company-require-match nil)
  :config
  (with-eval-after-load 'evil
    (define-key company-active-map (kbd "C-y") #'company-complete-selection)
    (define-key company-active-map (kbd "RET") nil)))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-doc-enable t)
  (company-box-scrollbar t))

(provide 'init-completion)
;;; init-completion.el ends herec
