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

(shl/set-font "Aporetic Sans Mono" "Aporetic Serif" 'semi-light 120)

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

(use-package gruber-darker-theme
  :ensure t
  :config (load-theme 'gruber-darker :no-confirm))

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

(provide 'init-ui)
;;; init-ui.el ends here
