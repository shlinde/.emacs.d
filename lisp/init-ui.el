;;; init-ui.el --- User Interface -*- lexical-binding: t; -*-
;;; Code: 

(require 'shl-core)
(require 'init-general)


;;;;; Fontaine (font configurations)
;; Read the manual: <https://protesilaos.com/emacs/fontaine>
(use-package fontaine
  :ensure t
  :hook
  ;; Persist the latest font preset when closing/starting Emacs.
  ((elpaca-after-init . fontaine-mode)
   (elpaca-after-init . shl/set-font))
  :general
  (shl/leader
    "t f" '(shl/fontaine-toggle :which-key "fontaine toggle"))
  :init
  (defun shl/set-font ()
    "Check if the current time is past 19:00 and run a function accordingly."
    (let ((current-hour (string-to-number (format-time-string "%H"))))
      (if (or (> current-hour 18) (< current-hour 5))
          (fontaine-set-preset 'regular-dark)
	(fontaine-set-preset 'regular-light))))
  (defun shl/fontaine-toggle ()
    "Toggle between =regular-light' and =regular-dark' fontaine presets."
    (interactive)
    (let* ((current (and (boundp 'fontaine-current-preset) fontaine-current-preset))
           (next (if (eq current 'regular-light) 'regular-dark 'regular-light)))
      (fontaine-set-preset next)
      (message "Fontaine preset set to: %s" next)))

  :config
  ;; And this is for Emacs28.
  (setq-default text-scale-remap-header-line t)

  ;; This is the default value.  Just including it here for
  ;; completeness.
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; The font family is my design: <https://github.com/protesilaos/aporetic>.
  (setq fontaine-presets
        '((small
           :default-height 80)
          (regular-dark
           :default-family shl-font
           :default-height 110
	   :default-weight semibold
           :fixed-pitch-family shl-mono-font
           :variable-pitch-family shl-variable-pitch-font)
          (regular-light
           :default-family shl-font
           :default-height 110
	   :default-weight normal
           :fixed-pitch-family shl-mono-font
           :variable-pitch-family shl-variable-pitch-font))))

;;;; Font Lock
(use-package font-lock
  :ensure nil
  :defer 1
  :custom
  ;; Max font lock decoration (set nil for less)
  (font-lock-maximum-decoration t)
  ;; No limit on font lock
  (font-lock-maximum-size nil))

;; Set default line spacing. If the value is an integer, it indicates
;; the number of pixels below each line. A decimal number is a scaling factor
;; relative to the current window's default line height. The setq-default
;; function sets this for all buffers. Otherwise, it only applies to the current
;; open buffer
(setq-default line-spacing 0.05)
(setq custom-safe-themes t)

(use-package doric-themes
  :ensure t
  :demand t
  :hook (elpaca-after-init . shl/set-theme)
  :init
  (defun shl/set-theme ()
    "Check if the current time is past 19:00 and run a function accordingly."
    (let ((current-hour (string-to-number (format-time-string "%H"))))
      (if (or (> current-hour 18) (< current-hour 5))
          (doric-themes-select 'doric-dark)
	(doric-themes-select 'doric-light))))
  :config
  ;; These are the default values.
  (setq doric-themes-to-toggle '(doric-light doric-dark))
  (setq doric-themes-to-rotate doric-themes-collection)
  (doric-themes-select 'doric-dark))


(use-package time
  :ensure nil
  :demand t
  :hook (elpaca-after-init . display-time-mode))

(use-package battery
  :ensure nil
  :demand t
  :hook (elpaca-after-init . display-battery-mode))

(use-package doom-modeline
  :ensure t
  :hook (elpaca-after-init . doom-modeline-mode))


(use-package diminish :ensure t)

(use-package spacious-padding
  :ensure t
  :hook (elpaca-after-init . spacious-padding-mode))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  )

(provide 'init-ui)
;;; init-ui.el ends here
