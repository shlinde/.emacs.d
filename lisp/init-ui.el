;;; init-ui.el --- User Interface -*- lexical-binding: t; -*-
;;; Code: 

(require 'shl-core)
(require 'init-general)

;;;;; Font configuration
(defun shl/set-font (default variable-pitch weight size)
  (progn
    (set-face-attribute 'default nil
			:width 'normal :weight weight
			:height size :font default)
    (set-face-attribute 'fixed-pitch nil
			:width 'normal :weight weight
			:height size :font default)
    (set-face-attribute 'variable-pitch nil
			:width 'normal :weight weight
			:height size :font variable-pitch)))

(shl/set-font "Aporetic Sans Mono" "Aporetic Serif" 'medium 120)

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
  ;; :hook (elpaca-after-init . shl/set-theme)
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
  (setq doric-themes-to-rotate doric-themes-collection))

(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn :no-confirm))

(use-package time
  :ensure nil
  :demand t
  :hook (elpaca-after-init . display-time-mode))

(use-package battery
  :ensure nil
  :demand t
  :hook (elpaca-after-init . (lambda ()
			       (when (not (string-match-p "N/A" (battery-format "%B" (funcall battery-status-function))))
				 display-battery-mode))))

;; A minimal modeline configuration inspired by Fabrice Bellard's philosophy.
(setq-default mode-line-format
  '("%e" ; Emacs an der Macht (auf Deutsch für "Emacs in power")
    mode-line-front-space
    ;; -- Buffer Status and Name --
    ;; Shows '--' if unmodified, '**' if modified
    (:propertize ("%*") face 'mode-line-buffer-id)
    ;; Shows the buffer name
    (:propertize (" %b ") face 'mode-line-buffer-id)
    " "
    ;; -- Major Mode --
    ;; Displays the major mode
    (:eval (capitalize (string-replace "-mode" "" (symbol-name major-mode))))
    "  "
    ;; -- Position in Buffer --
    ;; Shows the line and column number
    "L%l:%c"
    ;; -- Right-aligned ISO 8601 Datetime --
    mode-line-format-right-align
    mode-line-battery-status
    ;; Formats the current time as YYYY-MM-DD HH:MM:SS
    (:eval (format-time-string "%Y-%m-%d %H:%M:%S"))
    " "))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(provide 'init-ui)
;;; init-ui.el ends here
