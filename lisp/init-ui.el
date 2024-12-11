;;; init-ui.el --- Setup UI -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

;;; General Customizations
;; Get rid of the splash screen, and make the *scratch* buffer suitable for
;; writing.
(use-package emacs
  :ensure nil
  :config
  (setq inhibit-startup-message t
	inhibit-splash-screen t
	inhibit-startup-echo-area-message user-login-name
	inhibit-default-init t
	initial-major-mode 'fundamental-mode
	initial-scratch-message nil)
  (fset #'display-startup-echo-area-message #'ignore)

  ;; Get rid of the annoying system beep.
  (setq ring-bell-function 'ignore)

  ;; Scrolling
  (setq scroll-margin 0
	scroll-preserve-screen-position t
	next-screen-context-lines 3)

  ;; Reduce the clutter in the fringes; we'd like to reserve that space for more
  ;; useful information, like git-gutter and flycheck.
  (setq indicate-buffer-boundaries t
	indicate-empty-lines nil
	auto-window-vscroll nil)

  ;; Don't resize emacs in steps.
  (setq window-resize-pixelwise t
	frame-resize-pixelwise t)

  ;; Show tooltips when hovering over buttons
  (tooltip-mode 1)

  ;; No popup dialogs
  (setq use-dialog-box nil))

;;; Theme
(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-mode-line '(accented borderless)
	modus-themes-bold-constructs t
	modus-themes-italic-constructs t
	modus-themes-fringes 'subtle
	modus-themes-tabs-accented t
	modus-themes-paren-match '(bold intense)
	modus-themes-prompts '(bold intense)
	modus-themes-completions
        (quote ((matches . (extrabold underline))
                (selection . (semibold italic))))
	modus-themes-org-blocks 'tinted-background
	modus-themes-scale-headings t
	modus-themes-region '(bg-only)
	modus-themes-headings
	'((1 . (rainbow overline background 1.4))
          (2 . (rainbow background 1.3))
          (3 . (rainbow bold 1.2))
          (t . (semilight 1.1))))
  (modus-themes-select 'modus-vivendi-tinted))

;;; Font
(set-frame-font "ZedMono Nerd Font 12" nil t)

;;; Modeline
(use-package time
  :ensure nil
  :hook (after-init . display-time-mode))

(provide 'init-ui)
;;; init-ui.el ends here
