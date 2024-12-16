;;; init-ai.el --- AI Setup -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:


;;; Completion
(use-package corfu
  :ensure (corfu :files (:defaults "extensions/*.el"))
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
  :ensure t)

(provide 'init-completion)
;;; init-completion.el ends herec
